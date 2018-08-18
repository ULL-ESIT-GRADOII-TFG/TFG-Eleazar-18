{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Compiler.Prelude.OBool where

import qualified Data.Map            as M
import qualified Data.Text           as T

import           Compiler.Object
import           Compiler.Prelude.Th
import           Compiler.Types

methods
  :: (MemoryManagement mm, RawObj mm ~ Object mm)
  => M.Map T.Text ([Object mm] -> mm (Object mm))
methods = M.fromList
  [ ( "!"
    , $(normalize [| not |] (\_ty -> [t| Bool -> Bool |])))
  , (  "||"
    , $(normalize [| (||) |] (\_ty -> [t| Bool -> Bool -> Bool |])))
  , (  "&&"
    , $(normalize [| (&&) |] (\_ty -> [t| Bool -> Bool -> Bool |])))
  , (  "not"
    , $(normalize [| (not) |] (\_ty -> [t| Bool -> Bool |])))
  ]
