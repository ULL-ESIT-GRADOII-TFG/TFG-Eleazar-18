{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Config.Types where

import           Control.Monad
import           Data.Aeson
import qualified Data.Text           as T
import           Lens.Micro.Platform


data Config = Config
  { _commandShell :: Maybe FilePath
  , _defaultPath  :: Maybe FilePath
  , _prompt       :: String
  , _modules      :: [T.Text]
  }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                            (v .: "command_shell") <*>
                            (v .: "repl" >>= (.: "default_path")) <*>
                            (v .: "repl" >>= (.: "prompt")) <*>  -- TODO: try parse Code or string
                            (v .: "modules")

  parseJSON _ = mzero


makeLenses ''Config
