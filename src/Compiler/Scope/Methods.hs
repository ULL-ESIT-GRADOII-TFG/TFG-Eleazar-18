module Compiler.Scope.Methods where

import           Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Map as M
import           Lens.Micro.Platform

import Compiler.Scope.Types
import Compiler.Ast


-- | Initial scope
initialScope :: last -> Scope last
initialScope lastScope = Scope
  { _nextId = 0
  , _currentScope = ScopeInfo
    { _prevInfo = lastScope
    , _renameInfo = mempty
    }
  , _stackScope =  []
  }

-- | Create a temporal scope with a info
withNewScope :: a -> ScopeM a b -> ScopeM a b
withNewScope info body = do
  lastScope <- use currentScope
  stackScope %= (lastScope:)
  currentScope .= ScopeInfo info mempty
  value <- body
  first <- head <$> use stackScope
  currentScope .= first
  return value

-- | Generates a new ID
getNewId :: ScopeM a Word
getNewId = do
  newId <- use nextId
  nextId .= newId + 1
  return newId

-- | Add new variable name to scope and return its ID
addNewIdentifier :: T.Text -> ScopeM a Word
addNewIdentifier name = do
  newId <- getNewId
  currentScope.renameInfo %= M.insert name newId
  return newId

-- | Get a specific ID from variable name
getIdentifier :: T.Text -> ScopeM a Word
getIdentifier name = do
  renamer <- use $ currentScope.renameInfo
  case M.lookup name renamer of
    Just ref -> return ref
    Nothing -> do
      stack <- use stackScope
      maybe (throwError $ NoIdFound name) return (findInStack stack)

  where
    findInStack :: [ScopeInfo last] -> Maybe Word
    findInStack [] = Nothing
    findInStack (scope:xs) = case scope^.renameInfo & M.lookup name of
      Just ref -> Just ref
      Nothing -> findInStack xs

-- | Make a translation of variable names from AST, convert all to IDs and check rules
-- of scoping
scopingThroughtAST :: Expression a -> ScopeM a (ExpressionG a Word)
scopingThroughtAST expr =
  case expr of
    FunDecl args body info ->
      withNewScope info $ do
        argsId <- mapM addNewIdentifier args
        scopeBody <- scopingThroughtAST body
        return $ FunDecl argsId scopeBody info

    VarDecl name expr' info -> do
      nameId <- addNewIdentifier name
      withNewScope info $ do
        expr'' <- scopingThroughtAST expr'
        return $ VarDecl nameId expr'' info

    SeqExpr exprs info ->
      withNewScope info $ do
        expr' <- mapM scopingThroughtAST exprs
        return $ SeqExpr expr' info

    If condExpr trueExpr info -> do
      condExpr' <- withNewScope info $ scopingThroughtAST condExpr
      trueExpr' <- withNewScope info $ scopingThroughtAST trueExpr
      return $ If condExpr' trueExpr' info

    IfElse condExpr trueExpr falseExpr info -> do
      condExpr' <- withNewScope info $ scopingThroughtAST condExpr
      trueExpr' <- withNewScope info $ scopingThroughtAST trueExpr
      falseExpr' <- withNewScope info $ scopingThroughtAST falseExpr
      return $ IfElse condExpr' trueExpr' falseExpr' info

    For name iterExpr body info -> do
      iterExpr' <- withNewScope info $ scopingThroughtAST iterExpr
      nameId <- addNewIdentifier name
      body' <- withNewScope info $ scopingThroughtAST body
      return $ For nameId iterExpr' body' info


    Apply name args info -> do
      args' <- withNewScope info $ mapM scopingThroughtAST args
      nameId <- getIdentifier name
      return $ Apply nameId args' info

    Identifier name info -> do
      nameId <- getIdentifier name
      return $ Identifier nameId info

    Factor atom info ->
      return $ Factor atom info

