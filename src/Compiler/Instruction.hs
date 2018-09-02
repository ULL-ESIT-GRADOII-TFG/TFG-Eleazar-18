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
import           Data.Maybe
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector               as V
import           Lens.Micro.Platform       hiding (assign)

import           Compiler.Ast
import           Compiler.Object           ()
import           Compiler.Parser.Types
import           Compiler.Prettify
import           Compiler.Scope
import           Compiler.Scope.Ast
import           Compiler.Types
import           Compiler.World            ()


data Info = Info
  { _retrieveName :: IM.IntMap T.Text
  , _srcInfo      :: TokenInfo
  } deriving Show

tokToInfo :: TokenInfo -> Info
tokToInfo tok = Info mempty tok

type ProgInstr = Free Instruction

-- | Intermediate set of instructions.
data Instruction next
  = CreateVar !Info !Object !(Address -> next)
  -- ^ Build basic types
  | CallCommand !Info !PathVar ![Address] (Address -> next)
  -- ^ Make a call to and defined function
  | Assign !Info !PathVar !Address (Address -> next)
  -- ^ Assign an object to local variable
  | DropVar !Info !PathVar next
  -- ^ Remove a var from memory
  | GetRef !Info !PathVar (Address -> next)
  -- ^ Retrieve a object from a memory reference
  | Loop !Info !Address (Address -> ProgInstr Address) next
  -- ^ Loop over a object
  | Cond !Info !Address (ProgInstr Address) (ProgInstr Address) next
  -- ^ If sentence given a object
  | Noop
  deriving Functor

makeFree ''Instruction

instance Runnable ProgInstr StWorld Address where
  -- | Execute a sequence of instructions
  runProgram prog = flip iterM (fmap Just prog) $ \case
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
      addr <- setPathVar idObj (pure obj)
      next addr

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
        then runProgram trueNext >> next
        else runProgram falseNext >> next

    GetRef _ idObj next -> do
      (_o, addr) <- mkRef idObj :: StWorld (Object, Address)
      next addr
    Noop -> return Nothing

instance Pretty (ProgInstr Address) where
  -- iter :: Functor f => (f a -> a) -> Free f a -> a
  -- ProgInstr (Doc ann)
  -- flip iter (map pretty instrs) :: Instruction Doc ann -> ann
  pretty instrs = flip iter (fmap pretty instrs) $ \case
    CreateVar _ builder next ->
      let createVarPP = "CreateVar" <+> (pretty builder)
      in createVarPP <> line <> next 0

    CallCommand _ idFun args next ->
      let callDoc = "Call" <+> pAddr idFun <+> "With:" <+> pretty args
          docs = next 0
      in callDoc <> line <> docs

    Assign _ idObj accObject next ->
      let assignPP = "Assign" <+> pAddr idObj <+> pretty accObject
          docs = next 0
      in assignPP <> line <> docs

    DropVar _ idObj next ->
      let dropVarPP = "Drop" <+> pAddr idObj
          docs = next
      in dropVarPP <> line <> docs

    Loop _ accObject prog next ->
      let loopPP = "Loop" <+> pretty accObject
          bodyLoop = pretty (prog 0)
          docs     = next
      in loopPP <> line <> nest 2 bodyLoop <> line <> docs

    Cond _ objectCond trueNext falseNext next ->
      let condPP = "Cond" <+> pretty objectCond
          trueBody  = pretty trueNext
          falseBody = pretty falseNext
          docs      = next
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
    Noop -> "Noop"

newtype ExprSeq a = ExprSeq [Expression a]

instance Desugar ExprSeq Rn ScopeM ProgInstr Address where
  transform (ExprSeq exprs) = do
    instrs <- mapM transform exprs
    if null instrs then
      return noop
    else
      return $ do
        addrs <- sequence instrs
        return $ last addrs

-- | Transform AST to a simplified intermediate language, more related to
-- memory management
instance Desugar Expression Rn ScopeM ProgInstr Address where
  -- transform :: Expression ScopeInfoAST -> ScopeM (FreeT Instruction StWorld) Object
  transform expr = case expr of
    RnFunExpr idNames prog info -> do
      let info' = tokToInfo info
      let addresses = map idToAdr idNames
      body <- transform prog
      -- TODO: Generate Text -> Object
      return . createVar info' $ OFunc mempty addresses (\objs -> do
          let argsIDs = map simple addresses
          -- objs' <- mapM (getVal info' . simple) objs

          zipWithM_ (assign info') argsIDs objs
          obj <- body
          mapM_ (dropVar info') argsIDs
          return obj
        )

    RnVarExpr pathVar exprValue info -> do
      let info' = tokToInfo info
      instr <- transform exprValue
      -- newAddr <- getIdentifier (simplifiedAccessor acc) (info^.tokenInfoA)
      return $ do
        oldAddr <- instr
        assign info' pathVar oldAddr

    RnSeqExpr exprs info -> do
      let info' = tokToInfo info
      instrs <- mapM transform exprs
      if null instrs then
        return noop
      else
        return $ do
          addrs <- sequence instrs
          return $ last addrs

    RnMkScope scope exprs info -> do
      let info' = tokToInfo info
      instrs <- mapM transform exprs
      if null instrs then
        return noop
      else
        return $ do
          addrs <- sequence instrs
          mapM_ (dropVar info') (HM.elems $ scope^.renameInfoA)
          return $ last addrs

    RnIf condExpr prog info -> do
      let info' = tokToInfo info
      instr <- transform condExpr
      instrTrue <- transform prog
      -- Drop current scope defined vars
      return $ do
        value <- instr
        cond info' value instrTrue (createVar info' ONone)
        noop

    RnIfElse condExpr trueProg falseProg info -> do
      let info' = tokToInfo info
      condInstrs <- transform condExpr
      trueInstrs <- transform trueProg
      falseInstrs <- transform falseProg
      -- Drop current scope defined vars both
      return $ do
        val <- condInstrs
        cond info' val trueInstrs falseInstrs
        noop

    RnFor idName iterExpr prog info -> do
      let info' = tokToInfo info
      instrsIter <- transform iterExpr
      instrsBody <- transform prog
      -- Drop current scope defined vars and simple iter
      return $ do
        iter' <- instrsIter
        loop info' iter' (\val -> assign info' (simple $ idToAdr idName) val >> instrsBody)
        noop

    RnApply pathVar argsExpr info -> do
      let info' = tokToInfo info
      instrArgs <- mapM transform argsExpr
      return $ do
        args <- sequence instrArgs
        callCommand info' pathVar args

    RnIdentifier pathVar info -> do
      let info' = tokToInfo info
      return $ getRef info' pathVar

    RnFactor atom _info -> transform atom

-- | Transform literal data from AST to an object
instance Desugar Atom Rn ScopeM ProgInstr Address where
  -- transform :: Atom ScopeInfoAST -> ScopeM (Free Instruction) Object
  transform atom = case atom of
    ANone info         -> do
      let info' = tokToInfo info
      return $ createVar info' ONone
    ANum num info         -> do
      let info' = tokToInfo info
      return . createVar info' $ ONum num
    AStr str info          -> do
      let info' = tokToInfo info
      return . createVar info' $ OStr str
    ADecimal double info   -> do
      let info' = tokToInfo info
      return . createVar info' $ ODouble double
    ARegex _reg info        -> do
      let info' = tokToInfo info
      return . createVar info' $ ORegex undefined
    AShellCommand cmd info -> do
      let info' = tokToInfo info
      return . createVar info' $ OShellCommand cmd
    ABool bool info        -> do
      let info' = tokToInfo info
      return . createVar info' $ OBool bool
    AVector items info     -> do
      let info' = tokToInfo info
      addresses <- mapM transform items
      return $ do
        addresses' <- sequence addresses
        createVar info' . OVector $ V.fromList addresses'
    ADic items info      -> do
      let info' = tokToInfo info
      elems <- mapM (\(key, expr) -> (,) key <$> transform expr) items
      return $ do
        elems' <- mapM (\(name, val) -> (,) name <$> val) elems
        createVar info' . OObject Nothing $ HM.fromList elems'
    AClass name methods info -> do
      let info' = tokToInfo info
      methods' <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) methods
      return $ do
        methods'' <- mapM (\(n, val) -> (,) n <$> val) methods'
        createVar info' $ OClassDef name (HM.fromList methods'')

-------------------------------------------------------------------------------
-- * Utils
-------------------------------------------------------------------------------
-- TODO: Take address info
infoASTToInfo :: ScopeInfoAST -> ScopeM Info
infoASTToInfo scopeInfoAST =
  Info <$> return mempty <*> return (scopeInfoAST ^. tokenInfoA)

pAddr :: PathVar -> Doc ann
pAddr (PathVar addr vals) =
  let path = if not $ null vals then "-" <> (T.intercalate "." vals) else ""
  in  "#" <> pretty addr <> pretty path
