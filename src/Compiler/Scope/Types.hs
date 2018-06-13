module Compiler.Scope.Types where

import qualified Data.Text as T


data ScopeError
  = NoIdFound T.Text
  | InternalFail
  | ErrorClass
  | NoSavedAddressRef
  deriving Show
