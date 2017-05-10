{-# LANGUAGE OverloadedStrings #-}
module ScriptLang.Core where

import           Control.Monad.State.Strict
import           System.Console.Haskeline

import           ScriptLang.Env
import           ScriptLang.Interpreter


initialState :: IState
initialState = IState
  { _multiline = Nothing
  , _programState = initialEnv
  }

-- | Start the program searching a config with authentication tokens,
--   or request to user to log in
start :: IO ()
start = runInputT defaultSettings . flip evalStateT initialState $ do
  lift $ outputStrLn "Welcome to ScriptLang REPL. \nType :help for more information"
  repl
