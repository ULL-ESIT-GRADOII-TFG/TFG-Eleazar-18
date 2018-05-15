{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.OBool where

import qualified Data.Text              as T

import           Compiler.Prelude.Utils
import           Compiler.Types

methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "!"  -> Just $ normalizePure not
  "||" -> Just $ normalizePure' (||)
  "&&" -> Just $ normalizePure' (&&)
  _    -> Nothing
