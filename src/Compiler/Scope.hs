{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compiler.Scope where

import           Control.Monad.Except
import           Data.Default
import qualified Data.HashMap.Strict       as HM
import qualified Data.List.NonEmpty        as NL
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Lens.Micro.Platform

import           Compiler.Ast
import           Compiler.Error
import           Compiler.Identifier
import           Compiler.Parser.Types
import           Compiler.Prettify
import           Compiler.Types



instance Default Scope where
  def = Scope
    { _currentScope = ScopeInfo mempty
    , _stackScope   = []
    }

instance Naming ScopeM where
  newId name = addNewIdentifier (return name)
  getNewId = liftIO $ ID <$> getNewID
  findAddress' (name NL.:| names) = do
    renamer <- use $ currentScopeA.renameInfoA
    case HM.lookup name renamer of
      Just ref' -> return . Just $ PathVar (ref'^.refA) names
      Nothing   -> do
        stack <- use $ stackScopeA
        return (findInStack stack)

     where
      findInStack :: [ScopeInfo] -> Maybe PathVar
      findInStack []         = Nothing
      findInStack (scopeInfo':xs) = case scopeInfo'^.renameInfoA & HM.lookup name of
        Just (PathVar word _) -> Just $ PathVar word names
        Nothing               -> findInStack xs

instance Default ScopeInfo where
  def = ScopeInfo mempty

instance Prettify ScopeInfo where
  prettify (ScopeInfo hash) verbose =
    vsep [ "ScopeInfo { "
         , indent 2 (vcat (map (\(k,v) ->
                                  pretty k <+> "->" <+> prettify v verbose) $ HM.toList hash))
         , "}"
         ]

instance Default ScopeInfoAST where
  def = ScopeInfoAST def def

instance Prettify ScopeInfoAST where
  prettify scopeInfoAST verbose =
    vsep [ "ScopeInfoAST {"
         , indent 2 (prettify (_tokenInfo scopeInfoAST) verbose <> line <>
                     prettify (_scopeInfo scopeInfoAST) verbose)
         , "}"
         ]

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
      _addr <- catchError (getIdentifier (return name) info) $
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
      _addr <- catchError (getIdentifier accSimple info) $
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
      _addrRef <- getIdentifier accSimple info
      info' <- getScopeInfoAST info
      name' <- transform name
      return $ Apply name' args' info'

    Identifier name info -> do
      let accSimple = simplifiedAccessor name
      _addrRef <- getIdentifier accSimple info
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

flatScope :: Scope -> ScopeInfo
flatScope scope = ScopeInfo (
  (scope^.currentScopeA.renameInfoA) `mappend`
  mconcat (map _renameInfo (_stackScope scope)))

withScope :: ScopeInfo -> ScopeM a -> ScopeM a
withScope scope body = do
  addTopScope scope
  value <- body
  removeTopScope
  return value

addTopScope :: ScopeInfo -> ScopeM ()
addTopScope scope = do
  currentScope <- use currentScopeA
  stackScopeA %= (currentScope:)
  currentScopeA .= scope

removeTopScope :: ScopeM ()
removeTopScope = do
  (curScope:rest) <- use stackScopeA
  currentScopeA .= curScope
  stackScopeA   .= rest

-- | Create a temporal scope with a info
withNewScope :: ScopeM a -> ScopeM a
withNewScope = withScope def

-- | Add new variable name to scope and return its ID
addNewIdentifier :: NL.NonEmpty T.Text -> ScopeM PathVar
addNewIdentifier (name NL.:| names) = do
  idName <- getNewId
  let addr = idToAdr idName
  let pathVar = PathVar addr names
  currentScopeA.renameInfoA %= HM.insert name pathVar
  return pathVar

-- | Get a specific ID from variable name
getIdentifier :: NL.NonEmpty T.Text -> TokenInfo -> ScopeM PathVar
getIdentifier (name NL.:| names) info = do
  renamer <- use $ currentScopeA.renameInfoA
  case HM.lookup name renamer of
    Just ref' -> return $ PathVar (ref'^.refA) names
    Nothing   -> do
      stack <- use $ stackScopeA
      maybe (throwWithInfo info (NotDefinedObject name)) return (findInStack stack)

 where
  findInStack :: [ScopeInfo] -> Maybe PathVar
  findInStack []         = Nothing
  findInStack (scopeInfo':xs) = case scopeInfo'^.renameInfoA & HM.lookup name of
    Just (PathVar word _) -> Just $ PathVar word names
    Nothing               -> findInStack xs

-- | Generate ScopeInfoAST using the current scope info
getScopeInfoAST :: TokenInfo -> ScopeM ScopeInfoAST
getScopeInfoAST info = ScopeInfoAST info <$> use currentScopeA

getPathVar :: Show a => Accessor a -> ScopeInfoAST -> ScopeM PathVar
getPathVar acc scopeInfoAST =
  case HM.lookup (mainName acc) (scopeInfoAST^.scopeInfoA.renameInfoA) of
    Just (PathVar addr _) -> return $ PathVar addr (tailName acc)
    Nothing               -> throwWithInfo (scopeInfoAST^.tokenInfoA) NoSavedPathVar

-- | Simplies accesor to non empty list
simplifiedAccessor
  :: Accessor a -> NL.NonEmpty T.Text
simplifiedAccessor acc = case acc of
  Simple id' _tok -> return id'
  Dot id' acc' _  -> id' NL.<| simplifiedAccessor acc'

mainName :: Accessor a -> T.Text
mainName acc = case acc of
  Simple name _ -> name
  Dot name _ _  -> name

tailName :: Accessor a -> [T.Text]
tailName = NL.tail . simplifiedAccessor
