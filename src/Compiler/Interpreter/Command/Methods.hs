{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Command.Methods where

import           Control.Monad.Except
import           Data.List
import qualified Data.Map                     as M
import qualified Data.Text                    as T
import qualified Data.Text.Lazy.IO            as LT
import           Lens.Micro.Platform
import           System.Exit

import           Compiler.Instruction.Methods
import           Compiler.Interpreter.Utils
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
  [ ("mem", \_ -> use memory >>= liftIO . print) -- TODO: Make instances of prettify
  , ("instr", showInstructions)
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
  , "   :quit                -- Exits immediately form REPl"
  ]

showInstructions :: [T.Text] -> Interpreter ()
showInstructions [] = throwError $ Internal "Command just allow 1 arg"
showInstructions (name:_) = do
  -- Find into scope the ref -> search into world -> apply
  object <- getVar name
  case object of
    OFunc _ _ prog -> do
      instrs <- liftWorld $ do
        _ <- pprint prog
        instrs <- use $ debugProgram._1
        debugProgram .= ("", 0)
        return instrs
      liftIO $ LT.putStrLn instrs
    _  -> liftIO . putStrLn $ "Can't found `" ++ T.unpack name ++ "`"
