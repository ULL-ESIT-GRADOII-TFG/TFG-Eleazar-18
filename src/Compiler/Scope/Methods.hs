{-# LANGUAGE OverloadedStrings #-}
module Compiler.Scope.Methods where

import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map                   as M
import           Data.Monoid
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import           Compiler.Ast
import           Compiler.Instruction.Types
import           Compiler.Scope.Types


-- | Initial scope
initialScope :: Scope
initialScope = Scope
  { _nextId       = 0
  , _currentScope = ScopeInfo {_renameInfo = mempty}
  , _stackScope   = []
  }

-- | Create a temporal scope with a info
withNewScope :: ScopeM b -> ScopeM b
withNewScope body = do
  lastScope <- use currentScope
  stackScope %= (lastScope :)
  currentScope .= ScopeInfo mempty
  value <- body
  first <- head <$> use stackScope
  currentScope .= first
  return value

-- | Generates a new ID
getNewId :: ScopeM Word
getNewId = do
  newId <- use nextId
  nextId .= newId + 1
  return newId

-- | Add new variable name to scope and return its ID
addNewIdentifier :: [T.Text] -> ScopeM AddressRef
addNewIdentifier []     = throwError InternalFail
addNewIdentifier (name:names) = do
  newId <- getNewId
  let addr = AddressRef newId names
  currentScope . renameInfo %= M.insert name addr
  return addr

-- | Get a specific ID from variable name
getIdentifier :: [T.Text] -> ScopeM AddressRef
getIdentifier []     = throwError InternalFail
getIdentifier (name:names) = do
  renamer <- use $ currentScope . renameInfo
  case M.lookup name renamer of
    Just ref' -> return ref'
    Nothing  -> do
      stack <- use stackScope
      maybe (throwError $ NoIdFound name) return (findInStack stack)
 where
  findInStack :: [ScopeInfo] -> Maybe AddressRef
  findInStack []         = Nothing
  findInStack (scope:xs) = case scope ^. renameInfo & M.lookup name of
    Just (AddressRef word _) -> Just $ AddressRef word names
    Nothing                  -> findInStack xs

-- | Class renaming scope
-- TODO: Implement
scopingClassAST :: Statement a -> ScopeM ()
scopingClassAST (Class _name _expression _) = undefined
scopingClassAST _                           = undefined

-- | Make a translation of variable names from AST, convert all to IDs and check rules
-- of scoping
scopingThroughtAST :: Expression a -> ScopeM (ExpressionG Identity a AddressRef)
scopingThroughtAST expr = case expr of
  FunDecl args body info -> withNewScope $ do
    argsId    <- mapM (addNewIdentifier . (: [])) args
    scopeBody <- scopingThroughtAST body
    return $ FunDecl argsId scopeBody info

  VarDecl name expr' info -> do
    (exprAcc, addrRef) <- desugarAccessor name
    withNewScope $ do
      expr'' <- scopingThroughtAST expr'
      return $ SeqExpr [exprAcc, VarDecl (Identity addrRef) expr'' info] info

  SeqExpr exprs info -> withNewScope $ do
    expr' <- mapM scopingThroughtAST exprs
    return $ SeqExpr expr' info

  If condExpr trueExpr info -> do
    condExpr' <- withNewScope $ scopingThroughtAST condExpr
    trueExpr' <- withNewScope $ scopingThroughtAST trueExpr
    return $ If condExpr' trueExpr' info

  IfElse condExpr trueExpr falseExpr info -> do
    condExpr'  <- withNewScope $ scopingThroughtAST condExpr
    trueExpr'  <- withNewScope $ scopingThroughtAST trueExpr
    falseExpr' <- withNewScope $ scopingThroughtAST falseExpr
    return $ IfElse condExpr' trueExpr' falseExpr' info

  For name iterExpr body info -> do
    iterExpr' <- withNewScope $ scopingThroughtAST iterExpr
    nameId    <- addNewIdentifier [name]
    body'     <- withNewScope $ scopingThroughtAST body
    return $ For nameId iterExpr' body' info

  Apply name args info -> do
    args'             <- withNewScope $ mapM scopingThroughtAST args
    (exprAcc, accRef) <- desugarAccessor name
    return $ SeqExpr [exprAcc, Apply (Identity accRef) args' info] info

  Identifier name info -> do
    (exprAcc, accRef) <- desugarAccessor name
    return $ SeqExpr [exprAcc, Identifier (Identity accRef) info] info

  Factor atom info -> return $ Factor atom info

-- |
simplifiedAccessor
  :: Accessor a -> [(ScopeM (AddressRef -> AddressRef -> ExpressionG Identity a AddressRef), [T.Text])]
simplifiedAccessor acc = case acc of
  Simple id' tok            -> [(return $ \_ lastAddress -> Identifier (Identity lastAddress) tok, [id'])]
  Bracket id' expr mAcc tok ->
    let
      expr' = do
        expr'' <- scopingThroughtAST expr
        return $ \braceOP lastAddress ->
          Apply (Identity braceOP) [Identifier (Identity lastAddress) tok, expr''] tok
    in case mAcc of
      Just acc' ->
        (expr', [id']) : simplifiedAccessor acc'
      Nothing -> [(expr', [id'])]
  Dot id' acc' _ -> simplifiedAccessor acc' & _head._2 %~ (id':)

-- TODO: Add test
generateCode :: [(ScopeM (AddressRef -> AddressRef -> ExpressionG Identity a AddressRef), [T.Text])]
  -> ScopeM (ExpressionG Identity a AddressRef, AddressRef)
generateCode [] = error "Internal Error transform Accessor"
generateCode values = do
  braceOp <- getIdentifier ["self", "__brace__"]
  (exprs, lastRef) <- foldM (\(ret, lastRef) (scope, path) -> do
    expr <- scope
    id' <- use nextId
    lastAddress <-
      case lastRef of
        Just (AddressRef word _) -> return $ AddressRef word path
        Nothing                  -> addNewIdentifier path
    ref' <- addNewIdentifier ["$" <> (T.pack $ show id') :: T.Text]
    return (ret ++ [VarDecl (Identity ref') (expr braceOp lastAddress) undefined], (Just ref'))
    )
    ([], Nothing) values

  case lastRef of
    Just lastRef' -> return (SeqExpr exprs undefined, lastRef')
    Nothing       -> error "Internal Error"

desugarAccessor :: Accessor a
  -> ScopeM (ExpressionG Identity a AddressRef, AddressRef)
desugarAccessor =  generateCode . simplifiedAccessor
