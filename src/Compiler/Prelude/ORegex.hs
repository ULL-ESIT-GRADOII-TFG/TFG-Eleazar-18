{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Compiler.Prelude.ORegex where

import qualified Data.ByteString       as B
import qualified Data.Map              as M
import qualified Data.Text             as T
import           Text.Regex.PCRE.Light

import           Compiler.Object
import           Compiler.Prelude.Th
import           Compiler.Types

methods
  :: (MemoryManagement mm, RawObj mm ~ Object mm)
  => M.Map T.Text ([Object mm] -> mm (Object mm))
methods = M.fromList
  [ ( "match"
    , $(normalize [| \reg str -> match reg str [] |] (\_ty -> [t| Regex -> B.ByteString -> T.Text |])))
  , (  "capture_count"
    , $(normalize [| captureCount |] (\_ty -> [t| Regex -> Int |])))
 ]
