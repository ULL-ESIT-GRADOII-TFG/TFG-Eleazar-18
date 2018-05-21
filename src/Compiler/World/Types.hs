module Compiler.World.Types where

import qualified Data.Text as T

data WorldError
  = NotFoundObject
  | NotIterable
  | NotCallable
  | NumArgsMissmatch
  | NotImplicitConversion
  | WorldError T.Text
  deriving Show
