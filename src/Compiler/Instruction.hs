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
import           Lens.Micro.Platform        hiding (assign)
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
  | CallCommand !Info !PathVar ![Passing] (Passing -> next)
  -- ^ Make a call to and defined function
  | Assign !Info !PathVar !Passing (Address -> next)
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
  | Loop !Info !Passing (Address -> ProgInstr Passing) next
  -- ^ Loop over a object
  | Cond !Info !Passing (ProgInstr Passing) (ProgInstr Passing) (Passing -> next)
  -- ^ If sentence given a object
  | Noop
  deriving Functor

makeFree ''Instruction

instance Runnable ProgInstr StWorld where
  -- | Execute a sequence of instructions
  runProgram prog = flip iterM prog $ \case
    -- Used to create complex basic objects
    CreateVar _ obj next -> do
      addr <- newVar (wrap obj)
      next addr

    -- Find into world function and correspondent objects
    CallCommand _ idFun args next -> do
      retObj  <- call idFun args
      next retObj

    -- Make raw copy of value
    Assign _ idObj passing next -> do
      obj <- case passing of
        ByVal val -> return val
        ByRef ref -> unwrap <$> getVar ref

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
        then runProgram trueNext >>= next
        else runProgram falseNext >>= next

    GetRef _ idObj next -> do
      (_o, addr) <- mkRef idObj :: StWorld (Object, Address)
      next addr

    DirectRef _ addr next -> do
      rc <- getVar addr
      setVar addr (rc { _refCounter = _refCounter rc + 1 })
      next

    Noop -> return $ ByVal ONone

instance Pretty (ProgInstr Passing) where
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
      docs <- next $ ByRef adr
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
        <> indent 2 (pretty (prog  adr))
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
      adr <- fakeAdr
      docs <- next $ ByRef adr
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

instance Desugar ExprSeq Rn Identity ProgInstr Passing where
  transform (ExprSeq exprs) = do
    instrs <- mapM transform exprs
    if null instrs then
      return $ return (ByVal ONone)
    else
      return $ do
        addrs <- sequence instrs
        return $ last addrs

-- | Transform AST to a simplified intermediate language, more related to
-- memory management
instance Desugar Expression Rn Identity ProgInstr Passing where
  transform expr = case expr of
    RnFunExpr captureIds idNames prog info -> do
      let info' = tokToInfo info
      let addresses = map idToAdr idNames
      body <- transform prog
      return $ do
        capturedAddresses <- mapM (getRef info') captureIds
        return . ByVal $ OFunc capturedAddresses addresses (\objs -> do
          let argsIDs = map simple addresses
          addrs <- zipWithM (assign info') argsIDs objs
          retAddr <- body
          mapM_ (dropVar info') argsIDs
          return retAddr
          )


    RnVarExpr isNewVar pathVar exprValue info -> do
      let info' = tokToInfo info
      instr <- transform exprValue
      return $ do
        oldAddr <- instr
        addr <- assign info' pathVar oldAddr
        return $ ByRef addr
        -- if isNewVar then
        --   collectAddress addr
        -- else
        --   return addr


    RnSeqExpr exprs _info -> do
      -- let info' = tokToInfo info
      instrs <- mapM transform exprs
      if null instrs then
        return $ return (ByVal ONone)
      else
        return $ do
          addrs <- sequence instrs
          return $ last addrs

    RnMkScope scope exprs info -> do
      let info' = tokToInfo info
      instrs <- mapM transform exprs
      if null instrs then
        return $ return (ByVal ONone)
      else
        return $ do
          passings <- sequence instrs
          case last passings of
            ByRef ref -> do
              addr <- getRef info' (PathVar ref [])
              mapM_ (dropVar info') (HM.elems $ scope^.renameInfoA)
              return $ ByRef addr
            ByVal val -> do
              mapM_ (dropVar info') (HM.elems $ scope^.renameInfoA)
              return $ ByVal val


    RnIf condExpr prog info -> do
      let info' = tokToInfo info
      instr <- transform condExpr
      instrTrue <- transform prog
      return $ do
        value <- instr
        cond info' value instrTrue (return $ ByVal ONone)

    RnIfElse condExpr trueProg falseProg info -> do
      let info' = tokToInfo info
      condInstrs <- transform condExpr
      trueInstrs <- transform trueProg
      falseInstrs <- transform falseProg
      -- Drop current scope defined vars both
      return $ do
        val <- condInstrs
        cond info' val trueInstrs falseInstrs

    RnFor idName iterExpr prog info -> do
      let info' = tokToInfo info
      instrsIter <- transform iterExpr
      instrsBody <- transform prog
      -- Drop current scope defined vars and simple iter
      -- TODO: Remove inner collectLocalGC move to end
      return $ do
        iter' <- instrsIter
        loop info' iter' (\val -> assign info' (simple $ idToAdr idName) (ByRef val) >> instrsBody)
        return $ ByVal ONone

    RnApply pathVar argsExpr info -> do
      let info' = tokToInfo info
      instrArgs <- mapM transform argsExpr
      return $ do
        args <- sequence instrArgs
        callCommand info' pathVar args


    RnIdentifier pathVar info -> do
      let info' = tokToInfo info
      return $ do
        addr <- getRef info' pathVar
        return $ ByRef addr

    RnFactor atom _info -> do
      instrs <- transform atom
      return $ do
        instrs

    _ -> error "No supported AST Node"

-- | Transform literal data from AST to an object
instance Desugar Atom Rn Identity ProgInstr Passing where
  transform atom = case atom of
    ANone info         -> do
      let info' = tokToInfo info
      return . return $ ByVal ONone
    ANum num info         -> do
      let info' = tokToInfo info
      return . return . ByVal $ ONum num
    AStr str info          -> do
      let info' = tokToInfo info
      return . return . ByVal $ OStr str
    ADecimal double info   -> do
      let info' = tokToInfo info
      return . return . ByVal $ ODouble double
    ARegex reg info        -> do
      let info' = tokToInfo info
      return . return . ByVal $ ORegex reg (compile (T.encodeUtf8 reg) [])
    AShellCommand cmd info -> do
      let info' = tokToInfo info
      return . return . ByVal $ OShellCommand cmd
    ABool bool info        -> do
      let info' = tokToInfo info
      return . return . ByVal $ OBool bool
    AVector items info     -> do
      let info' = tokToInfo info
      passingArgs <- mapM transform items
      return $ do
        passingArgs' <- sequence passingArgs
        addresses' <- mapM (\passing -> do
                               case passing of
                                 ByVal val -> createVar info' val
                                 ByRef ref -> directRef info' ref >> return ref
                           ) passingArgs'
        return . ByVal . OVector $ V.fromList addresses'
    ADic items info      -> do
      let info' = tokToInfo info
      elems <- mapM (\(key, expr) -> (,) key <$> transform expr) items
      return $ do
        elems' <- mapM (\(n, val) -> (,) n <$> val) elems
        elems'' <- mapM (\(name, passing) -> (,) name <$> do
                            case passing of
                              ByVal val -> createVar info' val
                              ByRef ref -> directRef info' ref >> return ref
                       ) elems'
        return . ByVal . OObject Nothing $ HM.fromList elems''
    AClass name methods info -> do
      let info' = tokToInfo info
      methods' <- mapM (\(key, expr) -> (,) <$> return key <*> transform expr) methods
      return $ do
        methods'' <- mapM (\(n, val) -> (,) n <$> val) methods'
        methods''' <- mapM (\(name, passing) -> (,) name <$> do
                            case passing of
                              ByVal val -> createVar info' val
                              ByRef ref -> directRef info' ref >> return ref
                       ) methods''
        return . ByVal $ OClassDef name (HM.fromList methods''')

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
