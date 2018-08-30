{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compiler.Instruction where

import           Control.Monad
import           Control.Monad.Free        hiding (wrap)
import           Control.Monad.Free.TH
import qualified Data.HashMap.Strict       as HM
import qualified Data.IntMap               as IM
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector               as V
import           Lens.Micro.Platform       hiding (assign)

import           Compiler.Ast
import           Compiler.Object           ()
import           Compiler.Parser.Types
import           Compiler.Prettify
import           Compiler.Scope
import           Compiler.Types
import           Compiler.World            ()



data Info = Info
  { _retrieveName :: IM.IntMap T.Text
  , _srcInfo      :: TokenInfo
  } deriving Show

type ProgInstr = Free Instruction

-- | Intermediate set of instructions.
data Instruction next
  = CreateVar !Info !(ProgInstr Object) !(Address -> next)
  -- ^ Build basic types
  | CallCommand !Info !PathVar ![Object] (Object -> next)
  -- ^ Make a call to and defined function
  | Assign !Info !PathVar !Object (Object -> next)
  -- ^ Assign an object to local variable
  | DropVar !Info !PathVar next
  -- ^ Remove a var from memory
  | GetVal !Info !PathVar (Object -> next)
  -- ^ Retrieve a object from a memory reference
  | Loop !Info !Object (Object -> ProgInstr Object) next
  -- ^ Loop over a object
  | Cond !Info !Object (ProgInstr Object) (ProgInstr Object) (Object -> next)
  -- ^ If sentence given a object
  deriving Functor

makeFree ''Instruction

instance Runnable ProgInstr StWorld Object where
  -- | Execute a sequence of instructions
  runProgram = iterM $ \case
    -- Used to create complex basic objects
    CreateVar _ builder next -> do
      obj <- runProgram builder
      addr <- newVar (wrap obj)
      next addr

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
      (o, _addr) <- mkRef idObj
      next o

instance Prettify (ProgInstr Object) where
  prettify instrs verbose = flip iter (fmap (\obj -> prettify obj verbose) instrs) $ \case
    CreateVar _ builder next ->
      let createVarPP = "CreateVar" <+> (prettify builder verbose)
      in createVarPP <> line <> next 0

    CallCommand _ idFun args next ->
      let callDoc = "Call" <+> pAddr idFun <+> "With:" <+> pretty (map typeName args)
          docs = next ONone
      in callDoc <> line <> docs

    Assign _ idObj accObject next ->
      let assignPP = "Assign" <+> pAddr idObj <+> pretty (typeName accObject)
          docs = next ONone
      in assignPP <> line <> docs

    DropVar _ idObj next ->
      let dropVarPP = "Drop" <+> pAddr idObj
          docs = next
      in dropVarPP <> line <> docs

    Loop _ accObject prog next ->
      let loopPP = "Loop" <+> pretty (typeName accObject)
          bodyLoop = prettify (prog ONone) verbose
          docs     = next
      in loopPP <> line <> nest 2 bodyLoop <> line <> docs

    Cond _ objectCond trueNext falseNext next ->
      let condPP = "Cond" <+> pretty (typeName objectCond)
          trueBody  = prettify trueNext verbose
          falseBody = prettify falseNext verbose
          docs      = next ONone
      in condPP
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

    GetVal _ idObj next ->
      let getValPP = "GetVal " <+> pAddr idObj
          docs = next ONone
      in getValPP <> line <> docs

newtype ExprSeq a = ExprSeq [Expression a]

instance Desugar ExprSeq ScopeInfoAST ScopeM ProgInstr Object where
  transform (ExprSeq exprs) = foldl (>>) (return $ return ONone) (map transform exprs)

-- | Transform AST to a simplified intermediate language, more related to
-- memory management
instance Desugar Expression ScopeInfoAST ScopeM ProgInstr Object where
  -- transform :: Expression ScopeInfoAST -> ScopeM (FreeT Instruction StWorld) Object
  transform expr = case expr of
    FunExpr args prog info -> withScope (info^.scopeInfoA) $ do
      -- TODO: Generate Text -> Object
      info' <- infoASTToInfo info
      body <- transform prog
      addresses <- mapM (\acc -> do
                            addr <- getIdentifier (return acc) (info^.tokenInfoA)
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
      addr <- getIdentifier (simplifiedAccessor acc) (info^.tokenInfoA)
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
        mapM_ (dropVar info') (HM.elems $ info^.scopeInfoA.renameInfoA)
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
      addr <- getIdentifier (return acc) (info^.tokenInfoA)
      return $ do
        iter' <- instrsIter
        loop info' iter' (\val -> assign info' addr val >> instrsBody)
        return ONone

    Apply acc argsExpr info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instrArgs <- mapM transform argsExpr
      addr <- getIdentifier (simplifiedAccessor acc) (info^.tokenInfoA)
      return $ do
        args <- sequence instrArgs
        callCommand info' addr args

    Identifier acc info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      addr <- getIdentifier (simplifiedAccessor acc) (info^.tokenInfoA)
      return $ getVal info' addr

    Factor atom _info -> transform atom

-- | Transform literal data from AST to an object
instance Desugar Atom ScopeInfoAST ScopeM ProgInstr Object where
  -- transform :: Atom ScopeInfoAST -> ScopeM (Free Instruction) Object
  transform atom = case atom of
    ANone _            -> return $ return ONone
    ANum num _         -> return . return $ ONum num
    AStr str _          -> return . return $ OStr str
    ADecimal double _   -> return . return $ ODouble double
    ARegex _reg _        -> return . return $ ORegex undefined
    AShellCommand cmd _ -> return . return $ OShellCommand cmd
    ABool bool _        -> return . return $ OBool bool
    AVector items info     -> do
      info' <- infoASTToInfo info
      instrs <- mapM transform items
      return $ do
        objs <- mapM (createVar info') instrs
        return . OVector $ V.fromList objs
    ADic items info      -> do
      info' <- infoASTToInfo info
      values <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) items
      return $ do
        addrs <- mapM (createVar info' . snd) values
        return $ OObject Nothing . HM.fromList $ zip (map fst values) addrs
    AClass name methods info -> do
      info' <- infoASTToInfo info
      methods' <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) methods
      addr <- getIdentifier (return name) (info^.tokenInfoA)
      return $ do
        refFuncs <- mapM (createVar info' . snd) methods'
        return $ OClassDef name (addr^.refA) (HM.fromList $ zip (map fst methods) refFuncs)

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
