module Compiler.Scope.Methods where

import qualified Data.Text as T
import Lens.Micro.Platform

import Compiler.Scope.Types
import Compiler.Ast


{-
class ThroughAST a b m where
  throughtAST :: Expression a -> m (Expression b)

-}

newFromInfo :: a -> ScopeM a (ScopeInfo a)
newFromInfo lastInfo = do
  scope <- use lastScopeInfo
  return ScopeInfo
    { _prevInfo = lastInfo
    , _lastScope = scope
    , _renameInfo = mempty
    }

withScope :: ScopeInfo a -> ScopeM a b -> ScopeM a b
withScope scope body = do
  lastScopeInfo .= Just scope
  value <- body
  scope' <- use lastScopeInfo
  lastScopeInfo .= (scope' >>= _lastScope)
  return value

addNewIdentifier :: ScopeInfo a -> T.Text -> ScopeInfo a
addNewIdentifier = undefined

getIdentifier :: ScopeInfo a -> T.Text -> Word
getIdentifier = undefined

scopingThroughtAST :: Expression a -> ScopeM a (Expression (ScopeInfo a))
scopingThroughtAST expr =
  case expr of
    FunDecl args body info -> do
      scopeInfo <- newFromInfo info
      let scopeInfo' = foldl addNewIdentifier scopeInfo args
      scopeBody <- withScope scopeInfo' $ scopingThroughtAST body
      return $ FunDecl args scopeBody scopeInfo'

    VarDecl name expr' info -> do
      scopeInfo <- (`addNewIdentifier` name) `fmap` newFromInfo info
      expr'' <- withScope scopeInfo $ scopingThroughtAST expr'
      return $ VarDecl name expr'' scopeInfo

    SeqExpr exprs info -> do
      scopeInfo <- newFromInfo info
      exprs' <- withScope scopeInfo $ mapM scopingThroughtAST exprs
      return $ SeqExpr exprs' scopeInfo

    If condExpr trueExpr info -> do
      scopeInfo <- newFromInfo info
      condExpr' <- withScope scopeInfo $ scopingThroughtAST condExpr
      trueExpr' <- withScope scopeInfo $ scopingThroughtAST trueExpr
      return $ If condExpr' trueExpr' scopeInfo

    IfElse condExpr trueExpr falseExpr info -> do
      scopeInfo <- newFromInfo info
      condExpr' <- withScope scopeInfo $ scopingThroughtAST condExpr
      trueExpr' <- withScope scopeInfo $ scopingThroughtAST trueExpr
      falseExpr' <- withScope scopeInfo $ scopingThroughtAST falseExpr
      return $ IfElse condExpr' trueExpr' falseExpr' scopeInfo

    For name iterExpr body info -> do
      scopeInfo <- newFromInfo info
      iterExpr' <- withScope scopeInfo $ scopingThroughtAST iterExpr
      let scopeInfo' = addNewIdentifier scopeInfo name
      body' <- withScope scopeInfo' $ scopingThroughtAST body
      return $ For name iterExpr' body' scopeInfo


    Apply name args info -> do
      scopeInfo <- newFromInfo info
      args' <- withScope scopeInfo $ mapM scopingThroughtAST args
      return $ Apply name args' scopeInfo

    Identifier name info -> do
      scopeInfo <- newFromInfo info
      return $ Identifier name scopeInfo

    Factor atom info -> do
      scopeInfo <- newFromInfo info
      return $ Factor atom scopeInfo

