{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Compiler.Prelude.OShellCommand where

import           Control.Monad.Except
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           System.Process

import           Compiler.Error
import           Compiler.Object
import           Compiler.Types


methods
  :: (MemoryManagement mm, RawObj mm ~ Object mm)
  => M.Map T.Text ([Object mm] -> mm (Object mm))
methods = M.fromList
  [ ( "exec", execProcess)
  , ( "!", execProcess)
  ]

execProcess
  :: (MemoryManagement mm, RawObj mm ~ Object mm)
  => [Object mm]
  -> mm (Object mm)
execProcess objs = do
    let expectedArgs = 1
        givenArgs    = length objs
    case compare givenArgs expectedArgs of
      LT -> throw $ NumArgsMissmatch expectedArgs givenArgs
      GT -> throw $ NumArgsMissmatch expectedArgs givenArgs
      EQ -> do
        let [arg] = objs
        execProcess' arg

-- | Execute a command into the shell
execProcess'
  :: (MemoryManagement mm, RawObj mm ~ Object mm)
  => Object mm
  -> mm (Object mm)
execProcess' (OShellCommand text) = do
  value <- liftIO $ do
    (_, hout, _, _h) <-
      createProcess ((shell $ T.unpack text) { std_out = CreatePipe })
    maybe (return "") T.hGetContents hout
  return $ OStr value
execProcess' _ =
  throw $ WorldError "execProcess: Not called with OShellCommnad Object"
