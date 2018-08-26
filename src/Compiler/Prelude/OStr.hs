{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Prelude.OStr where

import qualified Data.Text           as T

import {-# SOURCE #-} Compiler.Object     ()
import           Compiler.Prelude.Th
import           Compiler.World      ()

methodsTh
  [ fn "++" [| T.append :: T.Text -> T.Text -> T.Text |]
  , fn "==" [| (==) :: T.Text -> T.Text -> Bool |]
  , fn "!=" [| (/=) :: T.Text -> T.Text -> Bool |]
  , fn "/=" [| (/=) :: T.Text -> T.Text -> Bool |]
  , fn "intercalate" [| T.intercalate :: T.Text -> [T.Text] -> T.Text |]
  , fn "reverse" [| T.reverse :: T.Text -> T.Text |]
  , fn "to_lower" [| T.toLower :: T.Text -> T.Text |]
  , fn "to_upper" [| T.toUpper :: T.Text -> T.Text |]
  , fn "to_title" [| T.toTitle :: T.Text -> T.Text |]
  , fn "take" [| flip T.take :: T.Text -> Int -> T.Text |]
  , fn "drop" [| flip T.drop :: T.Text -> Int -> T.Text |]
  -- Just when char type, it been implemented
  -- fn "center"      -> Just $ normalizePure'' T.center
  , fn "strip" [| T.strip :: T.Text -> T.Text |]
  , fn "strip_end" [| T.stripEnd :: T.Text -> T.Text |]
  , fn "strip_start" [| T.stripStart :: T.Text -> T.Text |]
  , fn "length" [| T.length :: T.Text -> Int |]
  , fn "tail" [| T.tail :: T.Text -> T.Text |]
  , fn "init" [| T.init :: T.Text -> T.Text |]
  , fn "null" [| T.null :: T.Text -> Bool |]
  ]
