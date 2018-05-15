{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.OShellCommand where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Free
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Process

import           Compiler.Prelude.Types
import           Compiler.Prelude.Utils
import           Compiler.Types

methods :: T.Text -> Maybe ([Object] -> Prog)
methods name = case name of
  "exec" -> Just $ normalize execProcess
  -- "!" -> Just $ normalizePure' (mappend :: V.Vector Object -> V.Vector Object -> V.Vector Object)
  _      -> Nothing

-- | Execute a command into the shell
execProcess :: Object -> FreeT Instruction StWorld Object
execProcess (OShellCommand text) = do
  value <- liftIO $ do
    (_, hout, _, h) <- createProcess ((shell $ T.unpack text) { std_out = CreatePipe })
    maybe (return "") T.hGetContents hout
  return $ OStr value
