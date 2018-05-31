{-# LANGUAGE TemplateHaskell #-}
module Compiler.Scope.Types where

import qualified Data.Text                  as T


data ScopeError
  = NoIdFound T.Text
  | InternalFail
  | ErrorClass
  deriving Show
