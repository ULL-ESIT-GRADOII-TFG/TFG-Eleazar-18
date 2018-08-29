{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Object.ODouble where


  -- TODO: Add negate operator
import {-# SOURCE #-} Compiler.Object     ()
import           Compiler.Prelude.Th
import           Compiler.World      ()


methodsTh
  [ fn "*" [| (*) :: Double -> Double -> Double |]
  , fn "/" [| (/) :: Double -> Double -> Double |]
  , fn "-" [| (-) :: Double -> Double -> Double |]
  , fn "+" [| (+) :: Double -> Double -> Double |]
  ]
