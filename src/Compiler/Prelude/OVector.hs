{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Prelude.OVector where

import qualified Data.Vector         as V

import {-# SOURCE #-} Compiler.Object     ()
import           Compiler.Prelude.Th
import           Compiler.Types
import           Compiler.World      ()


methodsTh
  [ fn "length" [| V.length :: V.Vector Object -> Int |]
  , fn "null" [| V.null :: V.Vector Object -> Bool |]
  , fn "head" [| V.head :: V.Vector Object -> Object |]
  , fn "last" [| V.last :: V.Vector Object -> Object |]
  , fn "slice" [| (\v a b -> V.slice a b v) :: V.Vector Object -> Int -> Int -> V.Vector Object |]
  , fn "take" [| flip V.take :: V.Vector Object -> Int -> V.Vector Object |]
  , fn "drop" [| flip V.drop :: V.Vector Object -> Int -> V.Vector Object |]
  , fn "__at__" [| (V.!?) :: V.Vector Object -> Int -> Object |]
  , fn "++" [| mappend :: V.Vector Object -> V.Vector Object -> V.Vector Object |]
  ]
