{-# LANGUAGE OverloadedStrings #-}
module Compiler.Scope.Types where

import qualified Data.Text      as T

import           Compiler.Error


data ScopeError
  = NotDefinedObject T.Text
  | InternalFail
  | ErrorClass
  | NoSavedAddressRef
  deriving Show

auxiliarScopeError err = case err of
    NotDefinedObject name -> (,) Error $
      "It wasn't found `" ++ T.unpack name ++ "` in the current scope."
    InternalFail      -> (,) Critical
      "A Internal Fail into Scope phase was happened. Report it."
    ErrorClass        -> (,) Error
      ""
    NoSavedAddressRef -> (,) Critical
      "Problem generating unique ids for variable name. Report it."

instance ReadeableError ScopeError where
  getMessage = snd . auxiliarScopeError

  getLevel = fst . auxiliarScopeError
