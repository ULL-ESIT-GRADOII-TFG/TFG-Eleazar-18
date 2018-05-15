{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.ODouble where

import qualified Data.Text              as T

import           Compiler.Prelude.Utils
import           Compiler.Types

  -- TODO: Add negate operator
methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "*" -> Just $ normalizePure' ((*) :: Double -> Double -> Double)
  "/" -> Just $ normalizePure' ((/) :: Double -> Double -> Double)
  "+" -> Just $ normalizePure' ((+) :: Double -> Double -> Double)
  "-" -> Just $ normalizePure' ((-) :: Double -> Double -> Double)
  _   -> Nothing
