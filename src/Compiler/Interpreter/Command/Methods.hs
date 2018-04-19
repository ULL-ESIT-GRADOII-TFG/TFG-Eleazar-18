{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Command.Methods where

import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Lens.Micro.Platform
import           System.Exit
import           Text.Groom

import           Compiler.Interpreter.Types


-- | Manage available commnads into interpreter
executeCommand :: T.Text -> [T.Text] -> Interpreter ()
executeCommand name args =
  case M.lookup name commands of
    Just command -> command args
    Nothing -> liftIO $ putStrLn "Command not found\nType :help to see available commands"

commands :: M.Map T.Text ([T.Text] -> Interpreter ())
commands = M.fromList
  [ ("mem", \_ -> use memory >>= liftIO . putStrLn . groom)
  , ("help", help)
  , ("quit", \_ -> liftIO exitSuccess)
  ]

-- | Show an informative text about REPL
-- TODO: Show doc about functions inside of interpreter (Methods, Class and types info)
help :: [T.Text] -> Interpreter ()
help _ = liftIO . putStrLn $ intercalate "\n"
  [ "HELP:"
  , "   :help   -- print this help itself"
  , "   :mem    -- Show current memory used"
  , "   :quit   -- Exits immediately form REPl"
  ]
