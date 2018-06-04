module Compiler.World.Types where

import qualified Data.Text as T

import Compiler.Scope.Types


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
  | ScopeError ScopeError
  deriving Show
