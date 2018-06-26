{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.OStr where

import qualified Data.Text              as T

import           Compiler.Prelude.Utils
import           Compiler.Types

methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "++"          -> Just $ normalizePure' T.append
  "=="          -> Just $ normalizePure' ((==) :: T.Text -> T.Text -> Bool)
  "!="          -> Just $ normalizePure' ((/=) :: T.Text -> T.Text -> Bool)
  "/="          -> Just $ normalizePure' ((/=) :: T.Text -> T.Text -> Bool)
  "intercalate" -> Just $ normalizePure' T.intercalate
  "reverse"     -> Just $ normalizePure T.reverse
  "to_lower"    -> Just $ normalizePure T.toLower
  "to_upper"    -> Just $ normalizePure T.toUpper
  "to_title"    -> Just $ normalizePure T.toTitle
  -- Just when char type it been implemented
  -- "center"      -> Just $ normalizePure'' T.center
  "take"        -> Just $ normalizePure' (flip T.take)
  "drop"        -> Just $ normalizePure' (flip T.drop)
  "strip"       -> Just $ normalizePure T.strip
  "strip_end"   -> Just $ normalizePure T.stripEnd
  "strip_start" -> Just $ normalizePure T.stripStart
  "length"      -> Just $ normalizePure T.length
  "tail"        -> Just $ normalizePure T.tail
  "init"        -> Just $ normalizePure T.init
  "null"        -> Just $ normalizePure T.null
  _             -> Nothing
