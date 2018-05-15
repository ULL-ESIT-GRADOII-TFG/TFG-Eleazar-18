{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.OStr where

import qualified Data.Text              as T

import           Compiler.Prelude.Utils
import           Compiler.Types

methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "++"          -> Just $ normalizePure' (T.append)
  "strip"       -> Just $ normalizePure (T.strip)
  "strip_end"   -> Just $ normalizePure (T.stripEnd)
  "strip_start" -> Just $ normalizePure (T.stripStart)
  _             -> Nothing
