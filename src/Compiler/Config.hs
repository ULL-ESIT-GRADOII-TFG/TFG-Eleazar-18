{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Config where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson              as A
import qualified Data.ByteString         as BS
import           Data.Default
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Yaml               as Y
import           Development.IncludeFile
import           System.Directory
import           System.FilePath

import           Compiler.Object         ()
import           Compiler.Types
import           Compiler.Utils


data Config = Config
  { _commandShell :: Maybe FilePath
  , _defaultPath  :: Maybe FilePath
  , _prompt       :: Maybe T.Text
  , _modules      :: [T.Text]
  }

instance Default Config where
  def = Config
    { _commandShell = Nothing
    , _defaultPath = Nothing
    , _prompt = Nothing
    , _modules =  []
    }

instance A.FromJSON Config where
  parseJSON (A.Object v) = Config <$>
                            (v A..: "command_shell") <*>
                            (v A..: "repl" >>= (A..: "default_path")) <*>
                            (v A..: "repl" >>= (A..: "prompt")) <*>
                            (v A..: "modules")

  parseJSON _ = mzero


makeSuffixLenses ''Config

-- $(includeFileInSource "prelude.sf" "preludeScript")
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
  loaded    <- loadConfigFile mCfgFile'
  if isNothing loaded
    then do
      putStrLn "Creating a default configuration"
      createDefaultConfig
      configFile' <- configFile
      loadConfigFile $ Just configFile'
    else return loaded

getConfigFilePath :: Maybe FilePath -> IO (Maybe FilePath)
getConfigFilePath mCfgFile = case mCfgFile of
  Just cfgFile -> do
    exist <- liftIO $ doesFileExist cfgFile
    if exist
      then return $ Just cfgFile
      else do
        liftIO
          $ putStrLn
              "Specified config file doesn't exist or it can't be accessed"
        liftIO $ putStrLn "Using XDG File"
        findConfigXDG
  Nothing -> findConfigXDG
  where
    findConfigXDG = do
      configFile' <- liftIO configFile
      exist       <- liftIO $ doesFileExist configFile'
      if exist
        then return $ Just configFile'
        else do
          liftIO
            $ putStrLn "XDG config file doesn't exist or it can't be accessed"
          return Nothing


-- | Try get configuration from default XDG directory. It get a file named
-- "scriptflow.yaml" in yaml format if file isn't found it returns False, True
-- in otherwise
loadConfigFile :: Y.FromJSON a => Maybe FilePath -> IO (Maybe a)
loadConfigFile mCfgFile = case mCfgFile of
  Just cfgFile -> do
    contents <- Y.decodeFileEither cfgFile -- :: IO (Either Y.ParseException Config)
    case contents of
      Right cFile       -> return $ Just cFile
      Left  parserError -> do
        liftIO $ putStrLn (Y.prettyPrintParseException parserError)
        return Nothing
  Nothing -> return Nothing

createDefaultConfig :: IO ()
createDefaultConfig = do
  configPath' <- configPath
  createDirectoryIfMissing False configPath'
  configFile' <- configFile
  exist       <- doesFileExist configFile'
  if exist
    then do
      putStr "Are you sure of overwrite your configuration? (Y/n) "
      response <- getLine
      when (response == "Y") $ BS.writeFile configFile' defaultConfig
    else BS.writeFile configFile' defaultConfig

-- | Allows load a specific configuration for object
loadSubConfig :: String -> StWorld Object
loadSubConfig name = do
  configPath' <- liftIO configPath
  mVal        <-
    liftIO $ loadConfigFile (Just (configPath' </> name <.> "yaml")) :: StWorld
      (Maybe Y.Value)
  case mVal of
    Just val -> toObject val
    Nothing  -> return ONone

-- | Save sub configuration
saveSubConfig :: String -> Object -> StWorld ()
saveSubConfig name obj = do
  configPath' <- liftIO configPath
  val <- fromObject obj :: StWorld Y.Value
  liftIO $ Y.encodeFile  (configPath' </> name <.> "yaml") val
