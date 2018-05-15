{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.OVector where

import qualified Data.Text              as T
import qualified Data.Vector            as V

import           Compiler.Prelude.Utils
import           Compiler.Types

methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "len" -> Just $ normalizePure (V.length :: V.Vector Object -> Int)
  "++"  -> Just $ normalizePure'
    (mappend :: V.Vector Object -> V.Vector Object -> V.Vector Object)
  _ -> Nothing
