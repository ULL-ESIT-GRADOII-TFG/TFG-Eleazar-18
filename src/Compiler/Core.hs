module Compiler.Core where

import           Control.Monad.State.Strict
import           System.Console.Haskeline

import           Compiler.Interpreter.Methods


-- | Start the program searching a config with authentication tokens,
--   or request to user to log in
start :: IO ()
start = runInputT defaultSettings . flip evalStateT initialState $ repl

