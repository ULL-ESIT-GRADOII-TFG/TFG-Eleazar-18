{-# LANGUAGE OverloadedStrings #-}
module Compiler.World.Types where

import qualified Data.Text            as T

import           Compiler.Error
import           Compiler.Scope.Types


data WorldError
  = NotFoundObject
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
    NotFoundObject             -> ""
    NotIterable                -> ""
    NotCallable                -> ""
    NumArgsMissmatch           -> ""
    NotImplicitConversion      -> ""
    ExcededRecursiveLimit      -> ""
    DropVariableAlreadyDropped -> ""
    NotExtensibleObject        -> ""
    WorldError _txt            -> ""
    ScopeError err'            -> getMessage err'
