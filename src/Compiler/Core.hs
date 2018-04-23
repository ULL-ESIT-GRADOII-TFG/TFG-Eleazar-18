module Compiler.Core where

import           Control.Monad.State.Strict
import Control.Monad.Except
import           System.Console.Haskeline

import           Compiler.Interpreter.Methods
import           Compiler.Prelude.Methods


-- | Start the program searching a config with authentication tokens,
--   or request to user to log in
start :: IO ()
start = do
  err <- runInputT defaultSettings . flip evalStateT initialState $ runExceptT (loadPrelude >> repl)
  case err of
    Right _ -> return ()
    Left err -> print err

