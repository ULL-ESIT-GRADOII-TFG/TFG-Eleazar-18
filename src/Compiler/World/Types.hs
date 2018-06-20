{-# LANGUAGE OverloadedStrings #-}
module Compiler.World.Types where

import qualified Data.Text            as T

import           Compiler.Error
import           Compiler.Scope.Types


data WorldError
  = NotFoundObject Word
  | NotIterable
  | NotCallable
  | NumArgsMissmatch
  | NotImplicitConversion
  | ExcededRecursiveLimit
  | DropVariableAlreadyDropped
  | NotExtensibleObject
  | WorldError T.Text
  | ScopeError (ErrorInfo ScopeError)
  deriving Show

instance ReadeableError WorldError where
  getMessage err = case err of
    NotFoundObject ref ->
      "It wasn't found ref `" ++ show ref ++ "` in memory."
    NotIterable                -> ""
    NotCallable                -> ""
    NumArgsMissmatch           -> ""
    NotImplicitConversion      -> ""
    ExcededRecursiveLimit      -> ""
    DropVariableAlreadyDropped -> ""
    NotExtensibleObject        -> ""
    WorldError _txt            -> ""
    ScopeError err'            -> getMessage err'
