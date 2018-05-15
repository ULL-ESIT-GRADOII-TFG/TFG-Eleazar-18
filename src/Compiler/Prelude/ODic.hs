{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.ODic where

import qualified Data.Text              as T

import           Compiler.Prelude.Utils
import           Compiler.Types

methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  _             -> Nothing
