module Compiler.Interpreter.Types where

import qualified Data.Text            as T

import           Compiler.World.Types


data InterpreterError
  = Compiling T.Text
  | Internal T.Text
  | WrapWorld WorldError
  deriving Show
