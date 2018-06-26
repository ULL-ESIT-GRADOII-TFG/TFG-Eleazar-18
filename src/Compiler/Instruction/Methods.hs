{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Compiler.Instruction.Methods where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import qualified Data.Map                 as M
import qualified Data.Vector              as V
import           Lens.Micro.Platform      hiding (assign)

import           Compiler.Ast
import           Compiler.Desugar.Types
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
        refFuncs <- lift $ mapM newObject oFuncs
        return $ OClassDef name (addr^.refA) (M.fromList $ zip (map fst methods) refFuncs)

-------------------------------------------------------------------------------
-- * Utils
-------------------------------------------------------------------------------
-- TODO: Take address info
infoASTToInfo :: ScopeInfoAST -> ScopeM Info
infoASTToInfo scopeInfoAST =
  Info <$> return mempty <*> return (scopeInfoAST^.tokenInfoA)
