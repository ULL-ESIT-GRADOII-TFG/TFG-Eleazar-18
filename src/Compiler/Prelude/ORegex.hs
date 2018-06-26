{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.ORegex where

import qualified Data.Text              as T
import           Text.Regex.PCRE.Light

import           Compiler.Prelude.Utils
import           Compiler.Types

methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "match"         -> Just $ normalizePure' (\reg str -> match reg str [])
  "capture_count" -> Just $ normalizePure captureCount
  _               -> Nothing
