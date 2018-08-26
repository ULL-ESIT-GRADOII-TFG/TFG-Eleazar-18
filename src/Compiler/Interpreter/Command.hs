{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Command where

import           Control.Monad.Except
import           Data.List
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc.Render.Text
import           Lens.Micro.Platform
import           System.Exit

import           Compiler.Instruction                  ()
import           Compiler.Interpreter
import           Compiler.Prettify
import           Compiler.Types


-- | Manage available commnads into interpreter
executeCommand :: T.Text -> [T.Text] -> Interpreter ()
executeCommand name args =
  case M.lookup name commands of
    Just command -> command args
    Nothing ->
      liftIO $ putStrLn "Command not found\nType :help to see available commands"

commands :: M.Map T.Text ([T.Text] -> Interpreter ())
commands = M.fromList
  [ ("mem", \_ -> do
      mem <- use memoryA
      verbosity <- use verboseLevelA
      liftIO . putDoc . (`prettify` verbosity) $ mem)
  , ("instr", showInstructions')
  , ("help", help)
  , ("quit", \_ -> liftIO exitSuccess)
  ]

-- TODO: Show doc about functions inside of interpreter (Methods, Class and
--       types info)
-- | Show an informative text about REPL
help :: [T.Text] -> Interpreter ()
help _ = liftIO . putStrLn $ intercalate "\n"
  [ "HELP:"
  , "   :help                -- print this help itself"
  , "   :mem                 -- Show current memory used"
  , "   :instr function_name -- Shows sequence of instruction a defined function"
  , "   :quit                -- Exits immediately form REPL"
  ]

showInstructions' :: [T.Text] -> Interpreter ()
showInstructions' [] = throwError $ Internal "Command just allow 1 arg"
showInstructions' (name:_) = do
  -- Find into scope the ref -> search into world -> apply
  object <- liftWorld $ unwrap <$> getVarWithName name
  case object of
    OFunc _ _ prog ->
      liftIO $ putDoc $ prettify (prog (repeat ONone)) 3
    _  -> liftIO . putStrLn $ "Can't found `" ++ T.unpack name ++ "`"
