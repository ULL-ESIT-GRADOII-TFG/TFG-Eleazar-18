{-# LANGUAGE TemplateHaskell #-}
module Compiler.Config.Methods where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import qualified Data.Yaml                  as Y
import qualified Data.ByteString            as BS
import           System.Directory
import           System.FilePath
import           Development.IncludeFile
import           System.Console.Haskeline

import           Compiler.Config.Types
import           Compiler.Interpreter.Types


$(includeFileInSource "prelude.sf" "preludeScript")
$(includeFileInSource "defaultConfig.yaml" "defaultConfig")

-- |
configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "scriptflow"

-- | Main config file
configFile :: IO FilePath
configFile = (</> "scriptflow.yaml") <$> configPath


setupConfig :: Interpreter ()
setupConfig = do
  loaded <- loadConfigFile
  when (not loaded) $ do
    liftIO $ putStrLn "Creating a default configuration"
    createDefaultConfig

-- | Try get configuration from default XDG directory. It get a file named "scriptflow.yaml" in yaml format
-- if file isn't found it returns False, True in otherwise
loadConfigFile :: Interpreter Bool
loadConfigFile = do
  configFile' <- liftIO configFile
  exist <- liftIO $ doesFileExist configFile'
  if exist then do
    contents <- liftIO (Y.decodeFileEither configFile' :: IO (Either Y.ParseException Config))
    case contents of
      Right cFile -> do
        -- TODO: Hacer uso del memory
        return True
      Left parserError ->
        liftIO $ putStrLn (Y.prettyPrintParseException parserError) >> return False
  else
    return False

createDefaultConfig :: Interpreter ()
createDefaultConfig = do
  configPath' <- liftIO configPath
  liftIO $ createDirectoryIfMissing False configPath'
  configFile' <- liftIO configFile
  exist <- liftIO $ doesFileExist configFile'
  if exist then do
    response <- lift . lift $ getInputLine "Are you sure of overwrite your configuration? (Y/n) "
    when (response == Just "Y") $ liftIO $ BS.writeFile configFile' defaultConfig
  else
    liftIO $ BS.writeFile configFile' defaultConfig
