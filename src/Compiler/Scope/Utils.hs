module Compiler.Scope.Utils where

import qualified Data.Text as T
import           Control.Monad.Except
import           Lens.Micro.Platform
import qualified Data.Map                   as M
import qualified Data.List.NonEmpty         as NL

import Compiler.Ast
import Compiler.Types hiding (scopeInfo)

-- | Create a temporal scope with a info
withNewScope :: ScopeM a -> ScopeM a
withNewScope body = do
  lastScope <- use currentScope
  stackScope %= (lastScope :)
  currentScope .= ScopeInfo mempty
  value <- body
  (curScope:rest) <- use stackScope
  currentScope .= curScope
  stackScope .= rest
  return value

-- | Generates a new ID
getNewId :: ScopeM Word
getNewId = do
  newId <- use nextId
  nextId .= newId + 1
  return newId

-- | Add new variable name to scope and return its ID
addNewIdentifier :: NL.NonEmpty T.Text -> ScopeM AddressRef
addNewIdentifier (name NL.:| names) = do
  newId <- getNewId
  let addr = AddressRef newId names
  currentScope . renameInfo %= M.insert name addr
  return addr

-- | Get a specific ID from variable name
getIdentifier :: NL.NonEmpty T.Text -> ScopeM AddressRef
getIdentifier (name NL.:|names) = do
  renamer <- use $ currentScope . renameInfo
  case M.lookup name renamer of
    Just ref' -> return $ AddressRef (ref'^.ref) names
    Nothing  -> do
      stack <- use stackScope
      maybe (throwError $ NoIdFound name) return (findInStack stack)
 where
  findInStack :: [ScopeInfo] -> Maybe AddressRef
  findInStack []         = Nothing
  findInStack (scopeInfo:xs) = case scopeInfo ^. renameInfo & M.lookup name of
    Just (AddressRef word _) -> Just $ AddressRef word names
    Nothing                  -> findInStack xs

-- | Generate ScopeInfoAST using the current scope info
getScopeInfoAST :: TokenInfo -> ScopeM ScopeInfoAST
getScopeInfoAST info = ScopeInfoAST info <$> use currentScope

getAddressRef :: Accessor a -> ScopeInfoAST -> StWorld AddressRef
getAddressRef = undefined

-- | Simplies accesor to non empty list
simplifiedAccessor
  :: Show a => Accessor a -> NL.NonEmpty T.Text
simplifiedAccessor acc = case acc of
  Simple id' _tok -> return id'
  Dot id' acc' _  -> id' NL.<| simplifiedAccessor acc'
