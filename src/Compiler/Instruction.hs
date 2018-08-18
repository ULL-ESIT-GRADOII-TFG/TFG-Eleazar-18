{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Compiler.Instruction where

import           Control.Monad
import           Control.Monad.Free.TH
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import qualified Data.IntMap               as IM
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector               as V
import           Lens.Micro.Platform       hiding (assign)

import           Compiler.Ast
import           Compiler.Object
import           Compiler.Parser.Types
import           Compiler.Scope
import           Compiler.Types
import           Compiler.World


type Prog = ProgInstr StWorld

data Info = Info
  { _retrieveName :: IM.IntMap T.Text
  , _srcInfo      :: TokenInfo
  } deriving Show

type ProgInstr mm = FreeT (Instruction mm) mm (RawObj mm)

-- | Intermediate set of instructions.
data Instruction mm next
  = CallCommand !Info !PathVar ![RawObj mm] (RawObj mm -> next)
  -- ^ Make a call to and defined function
  | Assign !Info !PathVar !(RawObj mm) (RawObj mm -> next)
  -- ^ Assign an object to local variable
  | DropVar !Info !PathVar next
  -- ^ Remove a var from memory
  | GetVal !Info !PathVar (RawObj mm -> next)
  -- ^ Retrieve a object from a memory reference
  | Loop !Info !(RawObj mm) (RawObj mm -> ProgInstr mm) next
  -- ^ Loop over a object
  | Cond !Info !(RawObj mm) (ProgInstr mm) (ProgInstr mm) (RawObj mm -> next)
  -- ^ If sentence given a object
  deriving Functor

makeFree ''Instruction

instance InstructionsLike StWorld where
  type Prog StWorld = FreeT (Instruction StWorld)
-- | Execute a sequence of instructions
  --runProgram :: Prog mm mm (RawObj mm) -> mm (RawObj mm)
  runProgram = iterT $ \case
    -- Find into world function and correspondent objects
    CallCommand _ idFun args next -> do
      retObj  <- call idFun args
      next retObj

    Assign _ idObj object next -> do
      -- object <- getObject accObject
      setPathVar idObj (pure object)
      next object -- TODO: debe retornar un ORef

    DropVar _ idObj next -> do
      addr <- snd <$> findPathVar idObj
      _ <- deleteVar addr
      next

    Loop _ accObject prog next -> do
      -- oIter <- getObject accObject
      _     <- mapOver accObject (runProgram . prog)
      next

    Cond _ objectCond trueNext falseNext next -> do
      bool <-  checkBool objectCond
      if bool
        then runProgram trueNext >>= next
        else runProgram falseNext >>= next

    GetVal _ idObj next -> do
      ref' <- unwrap . fst <$> findPathVar idObj
      next ref'

  --showInstructions :: Prog mm mm (RawObj mm) -> mm (Doc ())
  --pprint :: FreeT Instruction StWorld Object -> StWorld (Doc ())
  showInstructions instrs = (\f -> iterT f (fmap (const mempty) instrs)) $ \case
    CallCommand _ idFun args next -> do
      let callDoc = "Call" <+> pAddr idFun <+> "With:" <+> pretty (map typeName args)
      docs <- next ONone
      return $ callDoc <> line <> docs

    Assign _ idObj accObject next -> do
      let assignPP = "Assign" <+> pAddr idObj <+> pretty (typeName accObject)
      docs <- next ONone
      return $ assignPP <> line <> docs

    DropVar _ idObj next -> do
      let dropVarPP = "Drop" <+> pAddr idObj
      docs <- next
      return $ dropVarPP <> line <> docs

    Loop _ accObject prog next -> do
      let loopPP = "Loop" <+> pretty (typeName accObject)
      bodyLoop <- showInstructions $ prog ONone
      docs     <- next
      return $ loopPP <> line <> nest 2 bodyLoop <> line <> docs

    Cond _ objectCond trueNext falseNext next -> do
      let condPP = "Cond" <+> pretty (typeName objectCond)
      trueBody  <- showInstructions trueNext
      falseBody <- showInstructions falseNext
      docs      <- next ONone
      return
        $  condPP
        <> line
        <> "True Case:"
        <> line
        <> nest 2 trueBody
        <> line
        <> "False Case:"
        <> line
        <> nest 2 falseBody
        <> line
        <> docs

    GetVal _ idObj next -> do
      let getValPP = "GetVal " <+> pAddr idObj
      docs <- next ONone
      return $ getValPP <> line <> docs

newtype ExprSeq a = ExprSeq [Expression a]

instance Desugar ExprSeq ScopeInfoAST ScopeM (FreeT (Instruction StWorld) StWorld) (Object StWorld) where
  transform (ExprSeq exprs) = foldl (>>) (return $ return ONone) (map transform exprs)

-- | Transform AST to a simplified intermediate language, more related to
-- memory management
instance Desugar Expression ScopeInfoAST ScopeM (FreeT (Instruction StWorld) StWorld) (Object StWorld) where
  -- transform :: Expression ScopeInfoAST -> ScopeM (FreeT Instruction StWorld) Object
  transform expr = case expr of
    FunExpr args prog info -> withScope (info^.scopeInfoA) $ do
      -- TODO: Generate Text -> Object
      info' <- infoASTToInfo info
      body <- transform prog
      addresses <- mapM (\acc -> do
                            addr <- getIdentifier (return acc)
                            return $ addr^.refA
                        ) args

      return . return $ OFunc mempty addresses (\objs -> do
          let argsIDs = map simple addresses
          zipWithM_ (assign info') argsIDs objs
          obj <- body
          mapM_ (dropVar info') argsIDs
          return obj
        )

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
instance Desugar Atom ScopeInfoAST ScopeM (FreeT (Instruction StWorld) StWorld) (Object StWorld) where
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
        addrs <- lift $ mapM (newVar . pure) objs
        return . OVector $ V.fromList addrs
    ADic items _      -> do
      values <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) items
      return $ do
        objs <- mapM snd values
        addrs <- lift $ mapM (newVar . pure) objs
        return $ OObject Nothing . M.fromList $ zip (map fst values) addrs
    AClass name methods _ -> do
      methods' <- mapM (transform . snd) methods
      addr <- getIdentifier (return name)
      return $ do
        oFuncs <- sequence methods'
        refFuncs <- lift $ mapM (newVar . pure) oFuncs
        return $ OClassDef name (addr^.refA) (M.fromList $ zip (map fst methods) refFuncs)

-------------------------------------------------------------------------------
-- * Utils
-------------------------------------------------------------------------------
-- TODO: Take address info
infoASTToInfo :: ScopeInfoAST -> ScopeM Info
infoASTToInfo scopeInfoAST =
  Info <$> return mempty <*> return (scopeInfoAST ^. tokenInfoA)

pAddr :: PathVar -> Doc ()
pAddr (PathVar addr vals) =
  let path = if not $ null vals then "-" <> (T.intercalate "." vals) else ""
  in  "#" <> pretty addr <> pretty path
