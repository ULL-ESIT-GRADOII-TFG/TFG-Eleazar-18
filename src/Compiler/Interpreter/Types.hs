module Compiler.Interpreter.Types where

import qualified Data.Text as T


data InterpreterError
  = Compiling T.Text
  | Internal T.Text
  deriving Show
