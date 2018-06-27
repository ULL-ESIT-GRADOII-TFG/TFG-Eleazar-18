{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.OVector where

import qualified Data.Text              as T
import qualified Data.Vector            as V

import           Compiler.Prelude.Utils
import           Compiler.Types

methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "length" -> Just $ normalizePure (V.length :: V.Vector Object -> Int)
  "null" -> Just $ normalizePure (V.null :: V.Vector Object -> Bool)
  "__at__" -> Just $ normalizePure' ((V.!?) :: V.Vector Object -> Int -> Maybe Object)
  "head" -> Just $ normalizePure (V.head :: V.Vector Object -> Object)
  "last" -> Just $ normalizePure (V.last :: V.Vector Object -> Object)
  "slice" -> Just $ normalizePure'' (
      (\v a b -> V.slice a b v) :: V.Vector Object -> Int -> Int -> V.Vector Object)
  "take" -> Just $ normalizePure' (flip V.take :: V.Vector Object -> Int -> V.Vector Object)
  "drop" -> Just $ normalizePure' (flip V.drop :: V.Vector Object -> Int -> V.Vector Object)
  "++"  -> Just $ normalizePure'
    (mappend :: V.Vector Object -> V.Vector Object -> V.Vector Object)
  _ -> Nothing
