module Compiler.Interpreter.Types where

import qualified Data.Text            as T
import           Text.Parsec

import           Compiler.Error
import           Compiler.World.Types


data InterpreterError
  = Tokenizer T.Text
  | Parsing ParseError
  | Internal T.Text
  | WrapWorld (ErrorInfo WorldError)
  deriving Show
