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
  = CreateVar !Info !Object !(Address -> next)
  -- ^ Build basic types
  | CallCommand !Info !PathVar ![Address] (Address -> next)
  -- ^ Make a call to and defined function
  | Assign !Info !PathVar !Address next
  -- ^ Assign an object to local variable
  | DropVar !Info !PathVar next
  -- ^ Remove a var from memory
  | GetRef !Info !PathVar (Address -> next)
  -- ^ Retrieve a object from a memory reference
  | Loop !Info !Address (Address -> ProgInstr Address) next
  -- ^ Loop over a object
  | Cond !Info !Address (ProgInstr Address) (ProgInstr Address) (Address -> next)
  -- ^ If sentence given a object
  deriving Functor

makeFree ''Instruction

instance Runnable ProgInstr StWorld Address where
  -- | Execute a sequence of instructions
  runProgram = iterM $ \case
    -- Used to create complex basic objects
    CreateVar _ obj next -> do
      addr <- newVar (wrap obj)
      next addr

    -- Find into world function and correspondent objects
    CallCommand _ idFun args next -> do
      retObj  <- call idFun args
      next retObj

    Assign _ idObj address next -> do
      obj <- unwrap <$> getVar address
      _addr <- setPathVar idObj (pure obj)
      next

    DropVar _ idObj next -> do
      addr <- snd <$> findPathVar idObj
      _ <- deleteVar addr
      next

    Loop _ accObject prog next -> do
      _ <- mapOver accObject (\addr -> void . runProgram $ prog addr)
      next

    Cond _ addrObj trueNext falseNext next -> do
      bool <- checkBool addrObj
      if bool
        then runProgram trueNext >>= next
        else runProgram falseNext >>= next

    GetRef _ idObj next -> do
      (_o, addr) <- mkRef idObj :: StWorld (Object, Address)
      next addr

instance Prettify Address where
  prettify val _ = pretty val

instance Prettify (ProgInstr Address) where
  prettify instrs verbose = flip iter (fmap (\obj -> prettify obj verbose) instrs) $ \case
    CreateVar _ builder next ->
      let createVarPP = "CreateVar" <+> (prettify builder verbose)
      in createVarPP <> line <> next 0

    CallCommand _ idFun args next ->
      let callDoc = "Call" <+> pAddr idFun <+> "With:" <+> pretty args
          docs = next 0
      in callDoc <> line <> docs

    Assign _ idObj accObject next ->
      let assignPP = "Assign" <+> pAddr idObj <+> pretty accObject
          docs = next
      in assignPP <> line <> docs

    DropVar _ idObj next ->
      let dropVarPP = "Drop" <+> pAddr idObj
          docs = next
      in dropVarPP <> line <> docs

    Loop _ accObject prog next ->
      let loopPP = "Loop" <+> pretty accObject
          bodyLoop = prettify (prog 0) verbose
          docs     = next
      in loopPP <> line <> nest 2 bodyLoop <> line <> docs

    Cond _ objectCond trueNext falseNext next ->
      let condPP = "Cond" <+> pretty objectCond
          trueBody  = prettify trueNext verbose
          falseBody = prettify falseNext verbose
          docs      = next 0
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

    GetRef _ idObj next ->
      let getRefPP = "GetRef " <+> pAddr idObj
          docs = next 0
      in getRefPP <> line <> docs

newtype ExprSeq a = ExprSeq [Expression a]

instance Desugar ExprSeq ScopeInfoAST ScopeM ProgInstr Address where
  transform (ExprSeq exprs) = foldl (>>) (return $ return 0) (map transform exprs)

-- | Transform AST to a simplified intermediate language, more related to
-- memory management
instance Desugar Expression ScopeInfoAST ScopeM ProgInstr Address where
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

      return . createVar info' $ OFunc mempty addresses (\objs -> do
          let argsIDs = map simple addresses
          -- objs' <- mapM (getVal info' . simple) objs
          zipWithM_ (assign info') argsIDs objs
          obj <- body
          mapM_ (dropVar info') argsIDs
          return obj
        )

    VarExpr acc exprValue info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instr <- transform exprValue
      newAddr <- getIdentifier (simplifiedAccessor acc) (info^.tokenInfoA)
      return $ do
        oldAddr <- instr
        assign info' newAddr oldAddr
        return 0

    SeqExpr exprs info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      -- instrs <- mapM transform exprs
      -- return $ sequence instrs
      foldl (\scope expr' -> do
          instrs <- scope
          newinstrs <- transform expr'
          return (instrs >> newinstrs)
        ) (return $ createVar info' ONone) exprs

    MkScope exprs info -> withScope (info^.scopeInfoA) $ do
      info' <- infoASTToInfo info
      instrs <- foldl (\scope expr' -> do
          instrs <- scope
          newinstrs <- transform expr'
          return (instrs >> newinstrs)
        ) (return $ createVar info' ONone) exprs
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
        cond info' value instrTrue (createVar info' ONone)

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
        return 0

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
      return $ getRef info' addr

    Factor atom _info -> transform atom

-- | Transform literal data from AST to an object
instance Desugar Atom ScopeInfoAST ScopeM ProgInstr Address where
  -- transform :: Atom ScopeInfoAST -> ScopeM (Free Instruction) Object
  transform atom = case atom of
    ANone info         -> do
      info' <- infoASTToInfo info
      return $ createVar info' ONone
    ANum num info         -> do
      info' <- infoASTToInfo info
      return . createVar info' $ ONum num
    AStr str info          -> do
      info' <- infoASTToInfo info
      return . createVar info' $ OStr str
    ADecimal double info   -> do
      info' <- infoASTToInfo info
      return . createVar info' $ ODouble double
    ARegex _reg info        -> do
      info' <- infoASTToInfo info
      return . createVar info' $ ORegex undefined
    AShellCommand cmd info -> do
      info' <- infoASTToInfo info
      return . createVar info' $ OShellCommand cmd
    ABool bool info        -> do
      info' <- infoASTToInfo info
      return . createVar info' $ OBool bool
    AVector items info     -> do
      info' <- infoASTToInfo info
      addresses <- mapM transform items
      return $ do
        addresses' <- sequence addresses
        createVar info' . OVector $ V.fromList addresses'
    ADic items info      -> do
      info' <- infoASTToInfo info
      elems <- mapM (\(key, expr) -> (,) key <$> transform expr) items
      return $ do
        elems' <- mapM (\(name, val) -> (,) name <$> val) elems
        createVar info' . OObject Nothing $ HM.fromList elems'
    AClass name methods info -> do
      info' <- infoASTToInfo info
      methods' <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) methods
      addr <- getIdentifier (return name) (info^.tokenInfoA)
      return $ do
        methods'' <- mapM (\(n, val) -> (,) n <$> val) methods'
        createVar info' $ OClassDef name (addr^.refA) (HM.fromList methods'')

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
