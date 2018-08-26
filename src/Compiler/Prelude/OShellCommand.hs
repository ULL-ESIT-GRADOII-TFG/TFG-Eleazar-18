{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Prelude.OShellCommand where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.Process

import {-# SOURCE #-} Compiler.Object     ()
import           Compiler.Prelude.Th
import           Compiler.Types
import           Compiler.World      ()


methodsTh
  [ fn "exec" [| execProcess :: ShellType -> IO T.Text |]
  , fn "!" [| execProcess :: ShellType -> IO T.Text |]
  ]

-- | Execute a command into the shell
execProcess :: ShellType -> IO T.Text
execProcess text = do
  (_, hout, _, _h) <-
    createProcess ((shell . T.unpack $ unShell text) { std_out = CreatePipe })
  maybe (return "") T.hGetContents hout
