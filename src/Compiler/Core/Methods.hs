module Compiler.Core.Methods where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Version                 (showVersion)
import           System.Console.Haskeline

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
start _ = do
  err <- runInputT defaultSettings . flip evalStateT def $ runExceptT $ do
    loadPrelude
    setupConfig
    repl
  case err of
    Right _   -> return ()
    Left err' -> print err'
