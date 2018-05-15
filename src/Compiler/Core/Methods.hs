module Compiler.Core.Methods where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.Text.IO                 as T
import           Data.Version                 (showVersion)
import           System.Console.Haskeline
import           System.Directory

import           Compiler.Config.Methods
import           Compiler.Core.Types
import           Compiler.Interpreter.Methods
import           Compiler.Prelude.Methods
import           Compiler.Types

import           Paths_ScriptFlow


-- TODO: Use args passed to executable
-- | Start the program searching a config with authentication tokens,
--   or request to user to log in
start :: ArgsConfig -> IO ()
start Version = putStrLn $ "ScriptFlow v." ++ showVersion version
start (Interpreter inFile cfgFile initREPL _verbose) = do
  err <- runInputT defaultSettings . flip evalStateT def . runExceptT $ do
    loadPrelude
    setupConfig cfgFile
    case inFile of
      Just inFile' -> do
        exist <- liftIO $ doesFileExist inFile'
        if exist then do
          contents <- liftIO $ T.readFile inFile'
          compileSource contents inFile'
        else
          liftIO $ putStrLn $ "Can't be found file: " ++ inFile'
        when initREPL repl
      Nothing -> repl

  case err of
    Right _   -> return ()
    Left err' -> print err'
