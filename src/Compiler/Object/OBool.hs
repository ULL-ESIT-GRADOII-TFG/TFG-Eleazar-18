{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Object.OBool where


import {-# SOURCE #-} Compiler.Object     ()
import           Compiler.Prelude.Th
import           Compiler.World      ()


methodsTh
  [ fn "!" [| not :: Bool -> Bool |]
  , fn "||" [| (||) :: Bool -> Bool -> Bool |]
  , fn "&&" [| (&&) :: Bool -> Bool -> Bool |]
  , fn "not" [| not :: Bool -> Bool |]
  , fn "==" [| (==) :: Bool -> Bool -> Bool |]
  , fn "!=" [| (/=) :: Bool -> Bool -> Bool |]
  , fn "/=" [| (/=) :: Bool -> Bool -> Bool |]
  ]
