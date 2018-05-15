{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.ONum where

import qualified Data.Text              as T

import           Compiler.Prelude.Utils
import           Compiler.Types

  -- TODO: Add negate operator
methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "*" -> Just $ normalizePure' ((*) :: Int -> Int -> Int)
  "/" -> Just $ normalizePure' (div :: Int -> Int -> Int)
  "+" -> Just $ normalizePure' ((+) :: Int -> Int -> Int)
  "-" -> Just $ normalizePure' ((-) :: Int -> Int -> Int)
  _   -> Nothing
