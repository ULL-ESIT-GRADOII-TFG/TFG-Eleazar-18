module Compiler.World.Types where

import           Control.Monad.Except
import           Control.Monad.State.Strict

data Var

data World

data WorldError
  = NotFoundObject
  | NotIterable
  | NotCallable
  | NumArgsMissmatch
  | NotImplicitConversion

type StWorld = StateT World (ExceptT WorldError IO)
