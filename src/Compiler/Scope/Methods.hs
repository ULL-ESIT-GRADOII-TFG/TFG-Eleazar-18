{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Compiler.Scope.Methods where

import           Control.Monad.Except
import           Data.Default

import           Compiler.Ast
import           Compiler.Desugar.Types
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
    _address <- addNewIdentifier $ return name
    methds' <- withNewScope $ mapM (transform . funcToMethod) methds
    info' <- getScopeInfoAST info
    return $ VarExpr
      (Simple name info')
      (Factor (AClass name (zip (map _funName methds) methds') info') info') info'


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
      _addr <- catchError (getIdentifier accSimple) $
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

    MkScope exprs info -> withNewScope $ do
      expr' <- mapM transform exprs
      info' <- getScopeInfoAST info
      return $ MkScope expr' info'

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
    ANone i -> ANone <$> getScopeInfoAST i
    ANum val i -> ANum val <$> getScopeInfoAST i
    ADecimal val i -> ADecimal val <$> getScopeInfoAST i
    ARegex val i -> ARegex val <$> getScopeInfoAST i
    AShellCommand val i ->  AShellCommand val <$> getScopeInfoAST i
    AStr str i -> AStr str <$> getScopeInfoAST i
    ABool bool i-> ABool bool <$> getScopeInfoAST i
    AVector vals i -> AVector <$> mapM transform vals <*> getScopeInfoAST i
    AClass name vals i -> AClass name
      <$> mapM (\(key, val) -> (key,) <$> transform val) vals
      <*> getScopeInfoAST i
    ADic vals i -> ADic
      <$> mapM (\(key, val) -> (key,) <$> transform val) vals
      <*> getScopeInfoAST i

instance Desugar Accessor TokenInfo ScopeM Accessor ScopeInfoAST where
  -- transform :: Expression a -> ScopeM (Expression a)
  transform ast = case ast of
    Dot text acc info -> do
      acc' <- transform acc
      info' <- getScopeInfoAST info
      return $ Dot text acc' info'
    Simple text info  -> return $ Simple text (def {_tokenInfo = info})
