{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Object.ONum where

  -- TODO: Add negate operator
import {-# SOURCE #-} Compiler.Object     ()
import           Compiler.Prelude.Th
import           Compiler.World      ()


methodsTh
  [ fn "*" [| (*) :: Int -> Int -> Int |]
  , fn "/" [| div :: Int -> Int -> Int |]
  , fn "-" [| (-) :: Int -> Int -> Int |]
  , fn "+" [| (+) :: Int -> Int -> Int |]
  , fn ">" [| (>) :: Int -> Int -> Bool |]
  , fn "<" [| (<) :: Int -> Int -> Bool |]
  , fn "<=" [| (<=) :: Int -> Int -> Bool |]
  , fn ">=" [| (>=) :: Int -> Int -> Bool |]
  , fn "==" [| (==) :: Int -> Int -> Bool |]
  , fn "!=" [| (/=) :: Int -> Int -> Bool |]
  , fn "/=" [| (/=) :: Int -> Int -> Bool |]
  ]
