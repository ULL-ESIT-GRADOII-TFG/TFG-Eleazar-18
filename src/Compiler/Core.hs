{-# LANGUAGE OverloadedStrings #-}
module Compiler.Core where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.Text.IO                  as T
import           Data.Version                  (showVersion)
import           System.Console.Haskeline
import           System.Directory
import           Lens.Micro.Platform

import           Compiler.Config
import           Compiler.Interpreter
import           Compiler.Interpreter.Evaluate
import           Compiler.Prelude.Methods
import           Compiler.Types

import           Paths_ScriptFlow

-- | Arguments for executable
data ArgsConfig
  = Interpreter
    { inputFile  :: Maybe String
    , configPath :: Maybe String
    , startREPL  :: Bool
    , verbose    :: Int
    -- ^ Set a verbose level 0,1,2,3. See `prettify`
    }
  | Version

-- | Start the program searching a config with authentication tokens,
--   or request to user to log in
start :: ArgsConfig -> IO ()
start Version = putStrLn $ "ScriptFlow v." ++ showVersion version
start (Interpreter inFile cfgFile initREPL verbosity) = do
  let iState = def { _verboseLevel = verbosity }
  err <- runInputT defaultSettings . flip evalStateT iState . runExceptT $ do
    loadPrelude
    loadConfig cfgFile
    case inFile of
      Just inFile' -> do
        exist <- liftIO $ doesFileExist inFile'
        if exist
          then do
            contents <- liftIO $ T.readFile inFile'
            void $ compileSource contents inFile'
          else liftIO $ putStrLn $ "Can't be found file: " ++ inFile'
        when initREPL repl
      Nothing -> repl

  case err of
    Right _    -> return ()
    Left  err' -> print err'

loadConfig :: Maybe FilePath -> Interpreter ()
loadConfig cfgFile = do
  mCfg <- liftIO $ setupConfig cfgFile -- TODO
  case mCfg of
    Just cfg -> do
      configA .= cfg
      case cfg^.promptA of
        Just prompt -> do
          value <- compileSource prompt "** Prompt Code **"
          void $ liftWorld $ newVarWithName "__prompt__" value
        Nothing -> return ()
    Nothing -> return ()
