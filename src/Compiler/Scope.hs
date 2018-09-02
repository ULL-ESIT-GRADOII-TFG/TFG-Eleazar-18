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
import           Compiler.Scope.Ast
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

instance Pretty ScopeInfo where
  pretty (ScopeInfo hash) =
    vsep [ "ScopeInfo { "
         , indent 2 (vcat (map (\(k,v) ->
                                  pretty k <+> "->" <+> pretty v) $ HM.toList hash))
         , "}"
         ]

instance Default ScopeInfoAST where
  def = ScopeInfoAST def def

instance Pretty ScopeInfoAST where
  pretty scopeInfoAST =
    vsep [ "ScopeInfoAST {"
         , indent 2 (pretty (_tokenInfo scopeInfoAST) <> line <>
                     pretty (_scopeInfo scopeInfoAST))
         , "}"
         ]

instance Desugar Statement Tok ScopeM Expression Rn where
  -- transform :: Statement a -> ScopeM (Statement a)
  transform ast = case ast of
    ClassSt cls -> transform cls
    FunSt fun   -> transform fun
    Expr expr   -> transform expr

-- Convert to a constructor function
instance Desugar ClassDecl Tok ScopeM Expression Rn where
  -- transform :: ClassDecl a -> ScopeM (ClassDecl a)
  -- | Class renaming scope
  transform (ClassDecl name methds info) = do
    -- Generate class definition into scope
    RnVarExpr
      <$> (addNewIdentifier $ pure name)
      <*> (RnFactor
            <$> (AClass
                  <$> pure name
                  <*> (do
                          methds' <- withNewScope $ mapM (transform . funcToMethod) methds
                          -- TODO: Methods replicate warning
                          pure $ zip (map _funName methds) methds'
                      )
                  <*> pure info)
            <*> pure info)
      <*> pure info


funcToMethod :: FunDecl Tok -> FunDecl Tok
funcToMethod (FunDecl name args expr t) = FunDecl name ("self":args) expr t


instance Desugar FunDecl Tok ScopeM Expression Rn where
  -- transform :: FunDecl a -> ScopeM (Expression ScoepInfoAST)
  transform (FunDecl name args body info) = do
      RnVarExpr
        <$> (catchError (getIdentifier (return name) info) $
              \_ -> addNewIdentifier (return name))
        <*> transform (TokFunExpr args body info)
        <*> pure info


-- | Make a translation of variable names from AST, convert all to IDs and
-- check rules of scoping
instance Desugar Expression Tok ScopeM Expression Rn where
  -- transform :: Expression TokenInfo -> ScopeM (Expression ScopeInfoAST)
  transform ast = case ast of
    TokFunExpr args body info -> withNewScope $ do
      RnFunExpr
        <$> mapM (\arg -> do
                     pathArg <- addNewIdentifier $ return arg
                     return $ pathArg^.refA & adrToId
                 ) args
        <*> transform body
        <*> pure info

    TokVarExpr name expr' info -> do
      RnVarExpr
        <$> (do
                let accSimple = simplifiedAccessor name
                catchError (getIdentifier accSimple info) $
                  \_ -> addNewIdentifier accSimple
            )
        <*> withNewScope (transform expr')
        <*> pure info

    TokSeqExpr exprs info -> do
      RnSeqExpr
        <$> mapM transform exprs
        <*> pure info

    TokMkScope exprs info -> withNewScope $ do
      exprs' <- mapM transform exprs
      scopeInfo <- use currentScopeA
      return $ RnMkScope scopeInfo exprs' info

    TokIf condExpr trueExpr info ->
      RnIf
        <$> withNewScope (transform condExpr)
        <*> withNewScope (transform trueExpr)
        <*> pure info

    TokIfElse condExpr trueExpr falseExpr info ->
      RnIfElse
        <$> withNewScope (transform condExpr)
        <*> withNewScope (transform trueExpr)
        <*> withNewScope (transform falseExpr)
        <*> pure info

    TokFor name iterExpr body info -> do
      iterExpr' <- withNewScope $ transform iterExpr
      pathVar   <- addNewIdentifier $ return name
      body'     <- withNewScope $ transform body
      return $ RnFor (pathVar^.refA & adrToId) iterExpr' body' info

    TokApply name args info -> do
      RnApply
        <$> (do
                let accSimple = simplifiedAccessor name
                getIdentifier accSimple info
            )
        <*> withNewScope (mapM transform args)
        <*> pure info

    TokIdentifier name info -> do
      RnIdentifier
        <$> (do
                let accSimple = simplifiedAccessor name
                getIdentifier accSimple info
            )
        <*> pure info

    TokFactor atom info -> do
      RnFactor <$> transform atom <*> pure info

instance Desugar Atom Tok ScopeM Atom Rn where
  -- transform :: Expression a -> ScopeM (Expression a)
  transform atom = case atom of
    ANone loc -> return $ ANone loc
    ANum val loc -> return $ ANum val loc
    ADecimal val loc -> return $ ADecimal val loc
    ARegex val loc -> return $ ARegex val loc
    AShellCommand val loc -> return $ AShellCommand val loc
    AStr str loc -> return $ AStr str loc
    ABool bool loc -> return $ ABool bool loc
    AVector vals loc -> AVector <$> mapM transform vals <*> pure loc
    AClass name vals loc -> AClass name
      <$> mapM (\(key, val) -> (key,) <$> transform val) vals
      <*> pure loc
    ADic vals loc -> ADic
      <$> mapM (\(key, val) -> (key,) <$> transform val) vals
      <*> pure loc

-- instance Desugar Accessor TokenInfo ScopeM Accessor ScopeInfoAST where
--   -- transform :: Expression a -> ScopeM (Expression a)
--   transform ast = case ast of
--     Dot text acc info -> do
--       acc' <- transform acc
--       info' <- getScopeInfoAST info
--       return $ Dot text acc' info'
--     Simple text info  -> return $ Simple text (def {_tokenInfo = info})

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
