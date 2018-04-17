module Compiler.Interpreter.Command.Methods where

import           Control.Monad.IO.Class
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import           Compiler.Interpreter.Types


executeCommand :: T.Text -> [T.Text] -> Interpreter ()
executeCommand _ _ = do
  mem <- use memory
  liftIO $ print mem

