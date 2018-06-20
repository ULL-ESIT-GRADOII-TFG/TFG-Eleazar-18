module Compiler.Scope.Utils where

import           Control.Monad.Except
import qualified Data.List.NonEmpty   as NL
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Lens.Micro.Platform

import           Compiler.Ast
import           Compiler.Error
import           Compiler.Identifier
import           Compiler.Types

flatScope :: Scope -> ScopeInfo
flatScope scope = ScopeInfo $
  (scope^.currentScopeA.renameInfoA) `mappend`
  (mconcat (map _renameInfo (_stackScope scope)))

withScope :: ScopeInfo -> ScopeM a -> ScopeM a
withScope scope body = do
  currentScope <- use $ innerStateA.currentScopeA
  innerStateA.stackScopeA %= (currentScope:)
  innerStateA.currentScopeA .= scope
  value <- body
  (curScope:rest) <- use $ innerStateA.stackScopeA
  innerStateA.currentScopeA .= curScope
  innerStateA.stackScopeA   .= rest
  return value

-- | Create a temporal scope with a info
withNewScope :: ScopeM a -> ScopeM a
withNewScope = withScope def

-- | Add new variable name to scope and return its ID
addNewIdentifier :: NL.NonEmpty T.Text -> ScopeM AddressRef
addNewIdentifier (name NL.:| names) = do
  newId <- liftIO getNewID
  let addr = AddressRef (fromIntegral newId) names
  innerStateA.currentScopeA.renameInfoA %= M.insert name addr
  return addr

-- | Get a specific ID from variable name
getIdentifier :: NL.NonEmpty T.Text -> ScopeM AddressRef
getIdentifier (name NL.:| names) = do
  renamer <- use $ innerStateA.currentScopeA.renameInfoA
  case M.lookup name renamer of
    Just ref' -> return $ AddressRef (ref'^.refA) names
    Nothing  -> do
      stack <- use $ innerStateA.stackScopeA
      maybe (throw $ NoIdFound name) return (findInStack stack)

 where
  findInStack :: [ScopeInfo] -> Maybe AddressRef
  findInStack []         = Nothing
  findInStack (scopeInfo':xs) = case scopeInfo'^.renameInfoA & M.lookup name of
    Just (AddressRef word _) -> Just $ AddressRef word names
    Nothing                  -> findInStack xs

-- | Generate ScopeInfoAST using the current scope info
getScopeInfoAST :: TokenInfo -> ScopeM ScopeInfoAST
getScopeInfoAST info = ScopeInfoAST info <$> use (innerStateA.currentScopeA)

getAddressRef :: Show a => Accessor a -> ScopeInfoAST -> ScopeM AddressRef
getAddressRef acc scopeInfoAST =
  case M.lookup (mainName acc) (scopeInfoAST^.scopeInfoA.renameInfoA) of
    Just (AddressRef addr _) -> return $ AddressRef addr (tailName acc)
    Nothing                  -> throw NoSavedAddressRef

-- addIdentifier :: Accessor a -> AddressRef -> ScopeInfoAST -> ScopeInfoAST
-- addIdentifier acc addr = scopeInfo.renameInfo %~ M.insert (mainName acc) addr

-- newObject :: ScopeInfoAST -> ScopeInfoAST
-- newObject = scopeInfo.renameInfo %~ M.insert "__new__" (AddressRef 0 [])

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
