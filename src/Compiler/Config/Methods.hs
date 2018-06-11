{-# LANGUAGE TemplateHaskell #-}
module Compiler.Config.Methods where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.ByteString           as BS
import qualified Data.Yaml                 as Y
import           Development.IncludeFile
import           System.Console.Haskeline
import           System.Directory
import           System.FilePath

import           Compiler.Config.Types     ()
import           Compiler.Types


$(includeFileInSource "prelude.sf" "preludeScript")
$(includeFileInSource "defaultConfig.yaml" "defaultConfig")

-- |
configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "scriptflow"

-- | Main config file
configFile :: IO FilePath
configFile = (</> "scriptflow.yaml") <$> configPath

setupConfig :: Maybe FilePath -> Interpreter ()
setupConfig mCfgFile = do
  mCfgFile' <- getConfigFilePath mCfgFile
  loaded <- loadConfigFile mCfgFile'
  unless loaded $ do
    liftIO $ putStrLn "Creating a default configuration"
    createDefaultConfig
    configFile' <- liftIO configFile
    _ <- loadConfigFile $ Just configFile'
    return ()

getConfigFilePath :: Maybe FilePath -> Interpreter (Maybe FilePath)
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
loadConfigFile :: Maybe FilePath -> Interpreter Bool
loadConfigFile mCfgFile =
  case mCfgFile of
    Just cfgFile -> do
      contents <- liftIO
        (Y.decodeFileEither cfgFile :: IO (Either Y.ParseException Config))
      case contents of
        Right cFile -> do
          config .= cFile
          return True
        Left parserError -> do
          liftIO $ putStrLn (Y.prettyPrintParseException parserError)
          return False
    Nothing -> return False

createDefaultConfig :: Interpreter ()
createDefaultConfig = do
  configPath' <- liftIO configPath
  liftIO $ createDirectoryIfMissing False configPath'
  configFile' <- liftIO configFile
  exist       <- liftIO $ doesFileExist configFile'
  if exist
    then do
      response <- lift . lift $ getInputLine
        "Are you sure of overwrite your configuration? (Y/n) "
      when (response == Just "Y") $ liftIO $ BS.writeFile
        configFile'
        defaultConfig
    else liftIO $ BS.writeFile configFile' defaultConfig
