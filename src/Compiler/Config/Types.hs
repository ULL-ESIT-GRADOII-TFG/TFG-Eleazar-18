{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Config.Types where

import           Control.Monad
import           Data.Aeson
import           Lens.Micro.Platform

import           Compiler.Interpreter.Methods (compileSourcePure)
import           Compiler.Types


instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                            (v .: "command_shell") <*>
                            (v .: "repl" >>= (.: "default_path")) <*>
                            (v .: "repl" >>= (.: "prompt")) <*>
                            (v .: "modules")

  parseJSON _ = mzero

instance FromJSON Prompt where
  parseJSON (String text) =
    case compileSourcePure text "**Config File**" of
      Left e  -> fail $ show e
      Right p -> return $ Prompt p
  parseJSON _ = mzero

makeLenses ''Config
