{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Compiler.Instruction.Methods where

import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Vector              as V
import           Formatting
import           Lens.Micro.Platform      hiding (assign)

import           Compiler.Ast
import           Compiler.Desugar.Types
import           Compiler.Object.Methods
import           Compiler.Scope.Utils
import           Compiler.Types
import           Compiler.World.Methods


newtype ExprSeq a = ExprSeq [Expression a]

instance Desugar ExprSeq ScopeInfoAST ScopeM (FreeT Instruction StWorld) Object where
  transform (ExprSeq exprs) = foldl (>>) (return $ return ONone) (map transform exprs)

-- | Transform AST to a simplified intermediate language, more related to
-- memory management
instance Desugar Expression ScopeInfoAST ScopeM (FreeT Instruction StWorld) Object where
  -- transform :: Expression ScopeInfoAST -> ScopeM (FreeT Instruction StWorld) Object
  transform expr = case expr of
    FunExpr args prog info -> withScope (info^.scopeInfoA) $ do
      -- TODO: Generate Text -> Object
      body <- transform prog
      addresses <- mapM (\acc -> do
                            addr <- getIdentifier (return acc)
                            return $ addr^.refA
                        ) args
      return . return $ OFunc mempty addresses body

    VarExpr acc exprValue info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instr <- transform exprValue
      addr <- getIdentifier $ simplifiedAccessor acc
      return $
        instr >>= assign info' addr

    SeqExpr exprs info -> withScope (info^.scopeInfoA) $ do
      _info' <- infoASTToInfo info
      foldl (\scope expr' -> do
          instrs <- scope
          newinstrs <- transform expr'
          return (instrs >> newinstrs)
        ) (return $ return ONone) exprs

    MkScope exprs info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instrs <- foldl (\scope expr' -> do
          instrs <- scope
          newinstrs <- transform expr'
          return (instrs >> newinstrs)
        ) (return $ return ONone) exprs
      return $ do
        val <- instrs
        mapM_ (dropVar info') (M.elems $ info^.scopeInfoA.renameInfoA)
        return val

    If condExpr prog info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instr <- transform condExpr
      instrTrue <- transform prog
      -- Drop current scope defined vars
      return $ do
        value <- instr
        cond info' value instrTrue (return ONone)

    IfElse condExpr trueProg falseProg info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      condInstrs <- transform condExpr
      trueInstrs <- transform trueProg
      falseInstrs <- transform falseProg
      -- Drop current scope defined vars both
      return $ do
        val <- condInstrs
        cond info' val trueInstrs falseInstrs

    For acc iterExpr prog info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instrsIter <- transform iterExpr
      instrsBody <- transform prog
      -- Drop current scope defined vars and simple iter
      addr <- getIdentifier (return acc)
      return $ do
        iter' <- instrsIter
        loop info' iter' (\val -> assign info' addr val >> instrsBody)
        return ONone

    Apply acc argsExpr info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instrArgs <- mapM transform argsExpr
      addr <- getIdentifier (simplifiedAccessor acc)
      return $ do
        args <- sequence instrArgs
        callCommand info' addr args

    Identifier acc info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      addr <- getIdentifier (simplifiedAccessor acc)
      return $ getVal info' addr

    Factor atom _info -> transform atom

-- | Transform literal data from AST to an object
instance Desugar Atom ScopeInfoAST ScopeM (FreeT Instruction StWorld) Object where
  -- transform :: Atom ScopeInfoAST -> ScopeM (FreeT Instruction StWorld) Object
  transform atom = case atom of
    ANone _            -> return $ return ONone
    ANum num _         -> return . return $ ONum num
    AStr str _          -> return . return $ OStr str
    ADecimal double _   -> return . return $ ODouble double
    ARegex _reg _        -> return . return $ ORegex undefined
    AShellCommand cmd _ -> return . return $ OShellCommand cmd
    ABool bool _        -> return . return $ OBool bool
    AVector items _     -> do
      instrs <- mapM transform items
      return $ do
        objs <- sequence instrs
        return . OVector $ V.fromList objs
    ADic items _      -> do
      values <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) items
      return $ do
        objs <- mapM snd values
        addrs <- lift $ mapM newObject objs
        return $ OObject Nothing . M.fromList $ zip (map fst values) addrs
    AClass name methods _ -> do
      methods' <- mapM (transform . snd) methods
      addr <- getIdentifier (return name)
      return $ do
        oFuncs <- sequence methods'
        return $ OClassDef name (addr^.refA) (M.fromList $ zip (map fst methods) oFuncs)

-- | Execute a sequence of instructions
runProgram :: FreeT Instruction StWorld Object -> StWorld Object
runProgram = iterT $ \case
  -- Find into world function and correspondent objects
  CallCommand _ idFun args next -> do
    retObj  <- callObject idFun args
    next retObj

  Assign _ idObj object next -> do
    -- object <- getObject accObject
    addObject idObj object
    next object -- TODO: quizas se deba retornar un ORef

  DropVar _ idObj next -> do
    dropVarWorld idObj
    next

  Loop _ accObject prog next -> do
    -- oIter <- getObject accObject
    _     <- mapObj accObject (runProgram . prog)
    next

  Cond _ objectCond trueNext falseNext next -> do
    bool <-  checkBool objectCond
    if bool
      then runProgram trueNext >>= next
      else runProgram falseNext >>= next

  GetVal _ idObj next -> do
    ref' <- findObject idObj
    next ref'

linePP :: LT.Text -> StWorld ()
linePP txt = do
  level <- use $ innerStateA.debugProgramA._2
  innerStateA.debugProgramA._1 %= \t -> t `mappend` LT.replicate (fromIntegral level) "  " `mappend` txt `mappend` "\n"

withLevel :: StWorld a -> StWorld ()
withLevel action = do
  (innerStateA.debugProgramA._2) += 1
  _ <- action
  (innerStateA.debugProgramA._2) -= 1

pAddr :: AddressRef -> String
pAddr (AddressRef addr vals) =
  let path = if not $ null vals then "-" ++ T.unpack (T.intercalate "." vals) else ""
  in "#" ++ show addr ++ path

-- TODO: Make pretty printer
-- TODO: Remove formatter and use prettify
pprint :: FreeT Instruction StWorld Object -> StWorld Object
pprint = iterT $ \case
  CallCommand _ idFun args next -> do
    linePP (format ("Call " % string % " With: " % shown) (pAddr idFun) args)
    next ONone

  Assign _ idObj accObject next -> do
    linePP (format ("Assign " % string % " " % shown) (pAddr idObj) accObject)
    next ONone

  DropVar _ idObj next -> do
    linePP (format ("Drop " % string) (pAddr idObj))
    next

  Loop _ accObject prog next -> do
    linePP (format ("Loop " % shown) accObject)
    withLevel . pprint $ prog ONone
    next

  Cond _ objectCond trueNext falseNext next -> do
    linePP (format ("Cond " % shown) objectCond)
    withLevel $ do
      linePP "True Case:"
      withLevel $ pprint trueNext
      linePP "False Case:"
      withLevel $ pprint falseNext
    next ONone

  GetVal _ idObj next -> do
    linePP (format ("GetVal " % string) (pAddr idObj))
    next ONone

-------------------------------------------------------------------------------
-- * Utils
-------------------------------------------------------------------------------
-- TODO: Take address info
infoASTToInfo :: ScopeInfoAST -> ScopeM Info
infoASTToInfo scopeInfoAST =
  Info <$> return mempty <*> return (scopeInfoAST^.tokenInfoA)
