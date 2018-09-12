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
import           Control.Monad.Free         hiding (wrap)
import           Control.Monad.Free.TH
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector                as V
import           Text.Regex.PCRE.Light

import           Compiler.Ast
import           Compiler.Object            ()
import           Compiler.Parser.Types
-- import           Compiler.Prettify
import           Compiler.Scope.Ast
import           Compiler.Types
import           Compiler.World             ()


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
  | CollectAddress Address (Address -> next)
  -- ^ Collect address to remove later
  | ApplyLocalGC next
  -- ^ Apply a local GC with collect variables
  | GetRef !Info !PathVar (Address -> next)
  -- ^ Retrieve a object from a memory reference
  | DirectRef !Info !Address next
  -- ^ Implies the address have been linked to another, without generate
  -- intermediate object to reference it
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

    Loop _ accObject body next -> do
      _ <- mapOver accObject (\addr -> void . runProgram $ body addr)
      next

    CollectAddress addr next -> do
      collectAddr addr
      next addr

    ApplyLocalGC next -> do
      removeLocal
      next

    Cond _ addrObj trueNext falseNext next -> do
      bool <- checkBool addrObj
      if bool
        then runProgram trueNext >> next
        else runProgram falseNext >> next

    GetRef _ idObj next -> do
      (_o, addr) <- mkRef idObj :: StWorld (Object, Address)
      next addr

    DirectRef _ addr next -> do
      rc <- getVar addr
      setVar addr (rc { _refCounter = _refCounter rc + 1 })
      next

    Noop -> return Nothing

instance Pretty (ProgInstr Address) where
  -- TODO: The fake generate address doesn't match with a real behavior
  pretty instrs = flip evalState 0 $ flip iterM (fmap pretty instrs) $ \case
    CreateVar _ builder next -> do
      adr <- fakeAdr
      docs <- next adr
      return $
        pretty adr
        <+> "<- CreateVar"
        <+> (pretty builder)
        <> line
        <> docs

    CallCommand _ idFun args next -> do
      adr <- fakeAdr
      docs <- next adr
      return $
        pretty adr
        <+> "<- CallCommand"
        <+> pAddr idFun
        <+> "With:"
        <+> pretty args
        <> line
        <> docs

    Assign _ idObj accObject next -> do
      adr <- fakeAdr
      docs <- next adr
      return $
        pretty adr
        <+> "<- Assign"
        <+> pAddr idObj
        <+> pretty accObject
        <> line
        <> docs

    DropVar _ idObj next -> do
      docs <- next
      return $ "Drop" <+> pAddr idObj <> line <> docs

    Loop _ accObject prog next -> do
      adr <- fakeAdr
      docs <- next
      return $
        "Loop"
        <+> pretty accObject
        <> line
        <> indent 2 (pretty (prog adr))
        <> line
        <> docs

    CollectAddress addr next -> do
      docs <- next addr
      return $
        "CollectAddress" <+> pretty addr <> line <> docs

    ApplyLocalGC next -> do
      docs <- next
      return $
        "ApplyLocalGC" <> line <> docs

    Cond _ objectCond trueNext falseNext next -> do
      docs <- next
      return $
        "Cond"
        <+> pretty objectCond
        <> line
        <> "True Case:"
        <> line
        <> indent 2 (pretty trueNext)
        <> line
        <> "False Case:"
        <> line
        <> indent 2 (pretty falseNext)
        <> line
        <> docs

    GetRef _ idObj next -> do
      adr <- fakeAdr
      docs <- next adr
      return $
        pretty adr
        <+> "<- GetRef"
        <+> pAddr idObj
        <> line
        <> docs

    DirectRef _ addr next -> do
      docs <- next
      return $
        "DirectRef" <+> pretty addr <> line <> docs

    Noop -> return "Noop"

    where
      fakeAdr :: State Int Address
      fakeAdr = do
        old <- get
        put (old + 1)
        return $ Address old

newtype ExprSeq a = ExprSeq [Expression a]

instance Desugar ExprSeq Rn Identity ProgInstr Address where
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
instance Desugar Expression Rn Identity ProgInstr Address where
  transform expr = case expr of
    RnFunExpr captureIds idNames prog info -> do
      let info' = tokToInfo info
      let addresses = map idToAdr idNames
      body <- transform prog
      return $ do
        capturedAddresses <- mapM (getRef info') captureIds
        addr <- createVar info' $ OFunc capturedAddresses addresses (\objs -> do
          let argsIDs = map simple addresses
          addrs <- zipWithM (assign info') argsIDs objs
          mapM_ collectAddress addrs
          obj <- body
          -- mapM_ (dropVar info') argsIDs
          return obj
          )
        collectAddress addr


    RnVarExpr isNewVar pathVar exprValue info -> do
      let info' = tokToInfo info
      instr <- transform exprValue
      return $ do
        oldAddr <- instr
        addr <- assign info' pathVar oldAddr
        if isNewVar then
          collectAddress addr
        else
          return addr


    RnSeqExpr exprs _info -> do
      -- let info' = tokToInfo info
      instrs <- mapM transform exprs
      if null instrs then
        return noop
      else
        return $ do
          addrs <- sequence instrs
          return $ last addrs

    RnMkScope _scope exprs info -> do
      let info' = tokToInfo info
      instrs <- mapM transform exprs
      if null instrs then
        return noop
      else
        return $ do
          addrs <- sequence instrs
          -- mapM_ (dropVar info') (HM.elems $ scope^.renameInfoA)
          addr <- getRef info' (PathVar (last addrs) [])
          applyLocalGC
          collectAddress addr

    RnIf condExpr prog info -> do
      let info' = tokToInfo info
      instr <- transform condExpr
      instrTrue <- transform prog
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
      -- TODO: Remove inner collectLocalGC move to end
      return $ do
        iter' <- instrsIter
        loop info' iter' (\val -> assign info' (simple $ idToAdr idName) val >> instrsBody)
        noop

    RnApply pathVar argsExpr info -> do
      let info' = tokToInfo info
      instrArgs <- mapM transform argsExpr
      return $ do
        args <- sequence instrArgs
        addr <- callCommand info' pathVar args
        collectAddress addr


    RnIdentifier pathVar info -> do
      let info' = tokToInfo info
      return $ do
        addr <- getRef info' pathVar
        collectAddress addr

    RnFactor atom _info -> do
      instrs <- transform atom
      return $ do
        addr <- instrs
        collectAddress addr

    _ -> error "No supported AST Node"

-- | Transform literal data from AST to an object
instance Desugar Atom Rn Identity ProgInstr Address where
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
    ARegex reg info        -> do
      let info' = tokToInfo info
      return . createVar info' $ ORegex reg (compile (T.encodeUtf8 reg) [])
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
        mapM_ (directRef info') addresses'
        createVar info' . OVector $ V.fromList addresses'
    ADic items info      -> do
      let info' = tokToInfo info
      elems <- mapM (\(key, expr) -> (,) key <$> transform expr) items
      return $ do
        elems' <- mapM (\(name, val) -> (,) name <$> val) elems
        mapM_ (directRef info' . snd) elems'
        createVar info' . OObject Nothing $ HM.fromList elems'
    AClass name methods info -> do
      let info' = tokToInfo info
      methods' <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) methods
      return $ do
        methods'' <- mapM (\(n, val) -> (,) n <$> val) methods'
        mapM_ (directRef info' . snd) methods''
        createVar info' $ OClassDef name (HM.fromList methods'')

-------------------------------------------------------------------------------
-- * Utils
-------------------------------------------------------------------------------
-- TODO: Take address info
-- infoASTToInfo :: ScopeInfoAST -> ScopeM Info
-- infoASTToInfo scopeInfoAST =
--   Info <$> return mempty <*> return (scopeInfoAST ^. tokenInfoA)

pAddr :: PathVar -> Doc ann
pAddr (PathVar addr vals) =
  let path = if not $ null vals then "-" <> (T.intercalate "." vals) else ""
  in  "#" <> pretty addr <> pretty path
