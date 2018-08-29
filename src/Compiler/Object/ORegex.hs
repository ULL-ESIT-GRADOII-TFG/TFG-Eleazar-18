{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Object.ORegex where

import qualified Data.ByteString       as B
import qualified Text.Regex.PCRE.Light as R

import {-# SOURCE #-} Compiler.Object       ()
import           Compiler.Prelude.Th
import           Compiler.World        ()

methodsTh
  [ fn "match" [| (\reg str -> R.match reg str []) :: R.Regex -> B.ByteString -> Maybe [B.ByteString] |]
  , fn "capture_count" [| R.captureCount :: R.Regex -> Int |]
  ]
