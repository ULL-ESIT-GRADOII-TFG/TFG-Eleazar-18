{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Config where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString         as BS
import           Data.Default
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Yaml               as Y
import           Development.IncludeFile
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath


data Config = Config
  { _commandShell :: Maybe FilePath
  , _defaultPath  :: Maybe FilePath
  , _prompt       :: Maybe T.Text
  , _modules      :: [T.Text]
  }

-- newtype Prompt = Prompt { _unPrompt :: ScopeM Object }

instance Default Config where
  def = Config
    { _commandShell = Nothing
    , _defaultPath = Nothing
    , _prompt = Nothing -- Prompt . return $ OStr ">>> "
    , _modules =  []
}

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                            (v .: "command_shell") <*>
                            (v .: "repl" >>= (.: "default_path")) <*>
                            (v .: "repl" >>= (.: "prompt")) <*>
                            (v .: "modules")

  parseJSON _ = mzero

-- instance FromJSON Prompt where
--   parseJSON (String text) =
--     case compileSourcePure text "**Config File**" of
--       Left e  -> fail $ show e
--       Right p -> return $ Prompt $ do
--         p' <- p
--         return $ OFunc mempty [] (const p')
--   parseJSON _ = mzero

makeLenses ''Config

$(includeFileInSource "prelude.sf" "preludeScript")
$(includeFileInSource "defaultConfig.yaml" "defaultConfig")

-- |
configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "scriptflow"

-- | Main config file
configFile :: IO FilePath
configFile = (</> "scriptflow.yaml") <$> configPath

setupConfig :: Maybe FilePath -> IO (Maybe Config)
setupConfig mCfgFile = do
  mCfgFile' <- getConfigFilePath mCfgFile
  loaded <- loadConfigFile mCfgFile'
  if isNothing loaded then do
    putStrLn "Creating a default configuration"
    createDefaultConfig
    configFile' <- configFile
    loadConfigFile $ Just configFile'
  else
    return loaded

getConfigFilePath :: Maybe FilePath -> IO (Maybe FilePath)
getConfigFilePath mCfgFile =
  case mCfgFile of
    Just cfgFile -> do
      exist <- liftIO $ doesFileExist cfgFile
      if exist
        then
          return $ Just cfgFile
        else do
          liftIO $ putStrLn
            "Specified config file doesn't exist or it can't be accessed"
          liftIO $ putStrLn "Using XDG File"
          findConfigXDG
    Nothing -> findConfigXDG
  where
    findConfigXDG = do
      configFile' <- liftIO configFile
      exist       <- liftIO $ doesFileExist configFile'
      if exist then
        return $ Just configFile'
      else do
        liftIO $ putStrLn "XDG config file doesn't exist or it can't be accessed"
        return Nothing


-- | Try get configuration from default XDG directory. It get a file named
-- "scriptflow.yaml" in yaml format if file isn't found it returns False, True
-- in otherwise
loadConfigFile :: Maybe FilePath -> IO (Maybe Config)
loadConfigFile mCfgFile =
  case mCfgFile of
    Just cfgFile -> do
      contents <-
        Y.decodeFileEither cfgFile :: IO (Either Y.ParseException Config)
      case contents of
        Right cFile ->
          return $ Just cFile
        Left parserError -> do
          liftIO $ putStrLn (Y.prettyPrintParseException parserError)
          return Nothing
    Nothing -> return Nothing

createDefaultConfig :: IO ()
createDefaultConfig = do
  configPath' <- configPath
  createDirectoryIfMissing False configPath'
  configFile' <- configFile
  exist       <- doesFileExist configFile'
  if exist then do
    putStr "Are you sure of overwrite your configuration? (Y/n) "
    response <- getLine
    when (response == "Y") $ BS.writeFile
      configFile'
      defaultConfig
  else
    BS.writeFile configFile' defaultConfig
