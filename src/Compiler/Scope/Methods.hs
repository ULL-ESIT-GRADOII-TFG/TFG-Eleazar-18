{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Compiler.Scope.Methods where

import           Control.Monad.Except

import           Data.Default
--import qualified Data.IntMap                as IM
import qualified Data.List.NonEmpty     as NL
--import qualified Data.Map                   as M
import qualified Data.Text              as T

import           Compiler.Ast
import           Compiler.Desugar.Types
--import           Compiler.Instruction.Types
import           Compiler.Parser.Types
import           Compiler.Scope.Types
import           Compiler.Scope.Utils


instance Desugar Statement TokenInfo ScopeM Statement ScopeInfoAST where
  -- transform :: Statement a -> ScopeM (Statement a)
  transform ast = case ast of
    Import _ _  -> undefined
    ClassSt cls -> ClassSt <$> transform cls
    FunSt fun   -> FunSt <$> transform fun
    Expr expr   -> Expr <$>transform expr

instance Desugar ClassDecl TokenInfo ScopeM ClassDecl ScopeInfoAST where
  -- transform :: ClassDecl a -> ScopeM (ClassDecl a)
  -- TODO: Add syntax to build a class __init__
  -- TODO: Add a constructor to scope
  -- TODO:
  -- | Class renaming scope
  transform (ClassDecl name vars methds info) =
    -- AddressRef ref' _ <- addNewIdentifier $ return name

    -- typeDefinitions %= IM.insert (fromIntegral ref') (ClassDefinition name classDef)
    undefined

    -- (classDef, codeScope) <- withNewScope $ do
    --   codeScoped <- scopingThroughtAST expression
    --   currScope <- use $ currentScope.renameInfo
    --   -- Generate Free Program
    --   -- [(Ref, Free)]
    --   -- scopeInfo
    --   return (M.map _ref currScope, codeScoped)


    -- -- TODO: Add Constructor function. Peek for possible __init__ method
    -- typeDefinitions %= IM.insert (fromIntegral ref') (ClassDefinition name classDef)
    -- return codeScope

funcToMethod :: FunDecl a -> FunDecl a
funcToMethod (FunDecl name args expr t) = FunDecl name ("self":args) expr t

instance Desugar FunDecl TokenInfo ScopeM FunDecl ScopeInfoAST where
  -- transform :: FunDecl a -> ScopeM (ClassDecl a)
  transform = undefined

instance Desugar Expression TokenInfo ScopeM Expression ScopeInfoAST where
  -- transform :: Expression TokenInfo -> ScopeM (Expression ScopeInfoAST)
  transform ast = case ast of
    FunExpr args body info -> withNewScope $ do
      mapM_ (addNewIdentifier . return) args
      scopeBody <- transform body
      info' <- getScopeInfoAST info
      return $ FunExpr args scopeBody info'

    VarExpr name expr' info -> do
      let accSimple = simplifiedAccessor name
      _ <- catchError (getIdentifier accSimple) $
           \_ -> addNewIdentifier accSimple
      withNewScope $ do
        expr'' <- transform expr'
        name' <- transform name
        info' <- getScopeInfoAST info
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
      addrRef <- getIdentifier accSimple
      info' <- getScopeInfoAST info
      name' <- transform name
      return $ Apply name' args' info'

    Identifier name info -> do
      let accSimple = simplifiedAccessor name
      addrRef <- getIdentifier accSimple
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

-- -- | Make a translation of variable names from AST, convert all to IDs and check rules
-- -- of scoping
