{-# LANGUAGE OverloadedStrings #-}
module Compiler.Scope.Types where

import qualified Data.Text      as T

import           Compiler.Error


data ScopeError
  = NoIdFound T.Text
  | InternalFail
  | ErrorClass
  | NoSavedAddressRef
  deriving Show

instance ReadeableError ScopeError where
  getMessage err = case err of
    NoIdFound _id     -> ""
    InternalFail      -> ""
    ErrorClass        -> ""
    NoSavedAddressRef -> ""

  getLevel _err = Error
