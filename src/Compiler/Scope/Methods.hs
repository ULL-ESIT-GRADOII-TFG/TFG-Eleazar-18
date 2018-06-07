{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Compiler.Scope.Methods where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.IntMap                  as IM
import           Data.List
import qualified Data.Map                     as M
import           Lens.Micro.Platform

import           Compiler.Ast
import           Compiler.Desugar.Types
import           Compiler.Instruction.Methods
import           Compiler.Scope.Utils
import           Compiler.Types


instance Desugar Statement TokenInfo ScopeM Expression ScopeInfoAST where
  -- transform :: Statement a -> ScopeM (Statement a)
  transform ast = case ast of
    Import _ _  -> error "Not implemented yet"
    ClassSt cls -> transform cls
    FunSt fun   -> transform fun
    Expr expr   -> transform expr

-- Convert to a constructor function
instance Desugar ClassDecl TokenInfo ScopeM Expression ScopeInfoAST where
  -- transform :: ClassDecl a -> ScopeM (ClassDecl a)
  -- TODO: Methods replicate warning
  -- | Class renaming scope
  transform (ClassDecl name methds info) = do
    -- Generate class definition into scope
    address <- addNewIdentifier $ return name
    methds' <- withNewScope $ mapM (transform . funcToMethod) methds
    oFuncs <- forM methds' $ \func -> do
      object <- liftIO $ runExceptT (evalStateT (runProgram $ astToInstructions func) def)
      case object of
          Right fun@OFunc{} -> return fun
          _                 -> throwError ErrorClass -- TODO: Improve

    let classDef = ClassDefinition name (M.fromList $ zip (map (^.funName) methds) oFuncs)
    typeDefinitions %= IM.insert (fromIntegral $ address^.ref) classDef

    -- Creating a function to call __new__ special item. It allows create
    -- instances of this class. It uses the class name to find its definition
    body <- withNewScope $ do
      let initMethod = find (\funDecl -> funDecl^.funName == "__init__") methds
      let args = initMethod^._Just.funArgs
      let nameAST = Factor (ANum . fromIntegral $ address^.ref) info
      let argsAST = nameAST : map ((`Identifier` info ) . (`Simple` info)) args
      return $ FunExpr args (Apply (Simple "__new__" info) argsAST info) info

    transform $ VarExpr (Simple name info) body info

funcToMethod :: FunDecl a -> FunDecl a
funcToMethod (FunDecl name args expr t) = FunDecl name ("self":args) expr t


instance Desugar FunDecl TokenInfo ScopeM Expression ScopeInfoAST where
  -- transform :: FunDecl a -> ScopeM (Expression ScoepInfoAST)
  transform (FunDecl name args body info) = do
      _addr <- catchError (getIdentifier (return name)) $
        \_ -> addNewIdentifier (return name)
      info' <- getScopeInfoAST info
      body' <- withNewScope $ do
        mapM_ (addNewIdentifier . return) args
        info'' <- getScopeInfoAST info
        scopeBody <- transform body
        return $ FunExpr args scopeBody info''

      return $ VarExpr (Simple name info') body' info'


-- | Make a translation of variable names from AST, convert all to IDs and
-- check rules of scoping
instance Desugar Expression TokenInfo ScopeM Expression ScopeInfoAST where
  -- transform :: Expression TokenInfo -> ScopeM (Expression ScopeInfoAST)
  transform ast = case ast of
    FunExpr args body info -> withNewScope $ do
      mapM_ (addNewIdentifier . return) args
      info' <- getScopeInfoAST info
      scopeBody <- transform body
      return $ FunExpr args scopeBody info'

    VarExpr name expr' info -> do
      let accSimple = simplifiedAccessor name
      addr <- catchError (getIdentifier accSimple) $
           \_ -> addNewIdentifier accSimple
      info' <- getScopeInfoAST info
      withNewScope $ do
        expr'' <- transform expr'
        name' <- transform name
        return $ VarExpr name' expr'' info'

    SeqExpr exprs info -> do
      expr' <- mapM transform exprs
      info' <- getScopeInfoAST info
      return $ SeqExpr expr' info'

    MkScope exprs -> do
      expr' <- withNewScope $ mapM transform exprs
      return $ MkScope expr'

    If condExpr trueExpr info -> do
      condExpr' <- withNewScope $ transform condExpr
      trueExpr' <- withNewScope $ transform trueExpr
      info' <- getScopeInfoAST info
      return $ If condExpr' trueExpr' info'

    IfElse condExpr trueExpr falseExpr info -> do
      condExpr'  <- withNewScope $ transform condExpr
      trueExpr'  <- withNewScope $ transform trueExpr
      falseExpr' <- withNewScope $ transform falseExpr
      info' <- getScopeInfoAST info
      return $ IfElse condExpr' trueExpr' falseExpr' info'

    For name iterExpr body info -> do
      iterExpr' <- withNewScope $ transform iterExpr
      _         <- addNewIdentifier $ return name
      body'     <- withNewScope $ transform body
      info' <- getScopeInfoAST info
      return $ For name iterExpr' body' info'

    -- TODO:
    Apply name args info -> do
      args' <- withNewScope $ mapM transform args
      let accSimple = simplifiedAccessor name
      _addrRef <- getIdentifier accSimple
      info' <- getScopeInfoAST info
      name' <- transform name
      return $ Apply name' args' info'

    Identifier name info -> do
      let accSimple = simplifiedAccessor name
      _addrRef <- getIdentifier accSimple
      name' <- transform name
      info' <- getScopeInfoAST info
      return $ Identifier name' info'

    Factor atom info -> do
      info' <- getScopeInfoAST info
      (`Factor` info') <$> transform atom

instance Desugar Atom TokenInfo ScopeM Atom ScopeInfoAST where
  -- transform :: Expression a -> ScopeM (Expression a)
  transform atom = case atom of
    ANone -> return ANone
    ANum val -> return $ ANum val
    ADecimal val -> return $ ADecimal val
    ARegex val -> return $ ARegex val
    AShellCommand val -> return $ AShellCommand val
    AStr str -> return $ AStr str
    ABool bool -> return $ ABool bool
    AVector vals -> AVector <$> mapM transform vals
    ADic vals -> ADic <$> mapM (\(key, val) -> (key,) <$> transform val) vals

instance Desugar Accessor TokenInfo ScopeM Accessor ScopeInfoAST where
  -- transform :: Expression a -> ScopeM (Expression a)
  transform ast = case ast of
    Dot text acc info -> do
      acc' <- transform acc
      info' <- getScopeInfoAST info
      return $ Dot text acc' info'
    Simple text info  -> return $ Simple text (def {_tokenInfo = info})
