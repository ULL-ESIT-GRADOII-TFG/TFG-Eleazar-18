{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Compiler.Instruction.Methods where

import           Control.Monad
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


-- | Transform AST to a simplified intermediate language, more related to
-- memory management
instance Desugar Expression ScopeInfoAST ScopeM (FreeT Instruction StWorld) Object where
  -- transform :: Expression ScopeInfoAST -> ScopeM (FreeT Instruction StWorld) Object
  transform expr = case expr of
    FunExpr args prog info -> withScope (info^.scopeInfoA) $ do
      -- TODO: Generate Text -> Object
      body <- transform prog
      addresses <- mapM (\acc -> do
                            addr <- getAddressRef (Simple acc ()) info
                            return $ addr^.ref
                        ) args
      return . return $ OFunc mempty addresses body

    VarExpr acc exprValue info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instr <- transform exprValue
      addr <- getAddressRef acc info
      return $ do
        val <- instr
        assign info' addr val

    SeqExpr exprs info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instrs <- foldM (\_ expr' -> transform expr') (return ONone) exprs
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
      addr <- getAddressRef (Simple acc ()) info
      return $ do
        iter <- instrsIter
        loop info' iter (\val -> assign info' addr val >> instrsBody)
        return ONone

    Apply acc argsExpr info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instrArgs <- mapM transform argsExpr
      addr <- getAddressRef acc info
      return $ do
        args <- sequence instrArgs
        callCommand info' addr args

    Identifier acc info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      addr <- getAddressRef acc info
      return $ getVal info' addr

    Factor atom info -> transform atom

-- | Transform literal data from AST to an object
instance Desugar Atom ScopeInfoAST ScopeM (FreeT Instruction StWorld) Object where
  -- transform :: Atom ScopeInfoAST -> ScopeM (FreeT Instruction StWorld) Object
  transform atom = case atom of
    ANone             -> return $ return ONone
    ANum num          -> return . return $ ONum num
    AStr str          -> return . return $ OStr str
    ADecimal double   -> return . return $ ODouble double
    ARegex reg        -> return . return $ ORegex undefined
    AShellCommand cmd -> return . return $ OShellCommand cmd
    ABool bool        -> return . return $ OBool bool
    AVector items     -> do
      instrs <- mapM transform items
      return $ do
        objs <- sequence instrs
        return . OVector $ V.fromList objs
    ADic items       -> do
      values <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) items
      return $ do
        objs <- mapM snd values
        addrs <- lift $ mapM newObject objs
        return $ OObject Nothing . M.fromList $ zip (map fst values) addrs
    AClass name methods -> do
      methods' <- mapM (transform . snd) methods
      return $ do
        oFuncs <- sequence methods'
        return $ OClassDef name (M.fromList $ zip (map fst methods) oFuncs)

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
  level <- use $ debugProgramA._2
  debugProgramA._1 %= \t -> t `mappend` LT.replicate (fromIntegral level) "  " `mappend` txt `mappend` "\n"

withLevel :: StWorld a -> StWorld ()
withLevel action = do
  (debugProgramA._2) += 1
  _ <- action
  (debugProgramA._2) -= 1

pAddr :: AddressRef -> String
pAddr (AddressRef addr vals) =
  let path = if not $ null vals then "-" ++ T.unpack (T.intercalate "." vals) else ""
  in "#" ++ show addr ++ path

-- TODO: Make pretty printer
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
infoASTToInfo :: ScopeInfoAST -> ScopeM Info
infoASTToInfo = undefined

-- callCommand
--   :: (MonadFree Instruction m) => AddressRef -> [Object] -> m Object
-- callCommand info nameId objs = liftF (CallCommand nameId objs id)

-- (=:) :: (MonadFree Instruction m) => AddressRef -> Object -> m Object
-- nameId =: obj = liftF (Assign nameId obj id)

-- dropVar :: (MonadFree Instruction m) => AddressRef -> m ()
-- dropVar r = liftF (DropVar r ())

-- loop
--   :: (MonadFree Instruction m)
--   => Object
--   -> (Object -> FreeT Instruction StWorld Object)
--   -> m ()
-- loop obj prog = liftF (Loop obj prog ())

-- cond
--   :: (MonadFree Instruction m)
--   => Object
--   -> FreeT Instruction StWorld Object
--   -> FreeT Instruction StWorld Object
--   -> m Object
-- cond obj true false = liftF (Cond obj true false id)

-- getVal :: (MonadFree Instruction m) => AddressRef -> m Object
-- getVal obj = liftF (GetVal obj id)
