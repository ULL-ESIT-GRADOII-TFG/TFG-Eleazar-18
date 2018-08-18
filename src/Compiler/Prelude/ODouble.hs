{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Compiler.Prelude.ODouble where

import qualified Data.Map            as M
import qualified Data.Text           as T

  -- TODO: Add negate operator
import           Compiler.Object
import           Compiler.Prelude.Th
import           Compiler.Types

methods
  :: (MemoryManagement mm, RawObj mm ~ Object mm)
  => M.Map T.Text ([Object mm] -> mm (Object mm))
methods = M.fromList
  [ ( "*"
    , $(normalize [| (*) |] (\_ty -> [t| Double -> Double -> Double |])))
  , ( "()"
    , $(normalize [| (/) |] (\_ty -> [t| Double -> Double -> Double |])))
  , ( "+"
    , $(normalize [| (+) |] (\_ty -> [t| Double -> Double -> Double |])))
  , ( "-"
    , $(normalize [| (+) |] (\_ty -> [t| Double -> Double -> Double |])))
 ]
