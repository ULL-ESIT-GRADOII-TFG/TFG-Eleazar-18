module Compiler.Error where

import qualified Data.Text as T


data ErrorLevel = Critical | Warning | Error deriving (Show, Eq)


class Error a where
  outputInterpreter :: a -> T.Text

  getLine :: a -> Int
  getColumn :: a -> Int
  getErrorMessage :: a -> T.Text
  getErrorClass :: a -> T.Text
  getLevel :: a -> ErrorLevel
