{-# LANGUAGE TemplateHaskell #-}
module Compiler.Interpreter.Types where

import           Control.Monad.State.Strict
import           Control.Monad.Except
import qualified Data.Text                  as T
import           Lens.Micro.Platform
import           System.Console.Haskeline   (InputT)

import           Compiler.World.Types


data InterpreterError
  = Compiling T.Text
  | Internal T.Text
  deriving Show

-- | Used to control flow of all interpreter
type Interpreter = ExceptT InterpreterError (StateT IState (InputT IO))

-- | Represent internal state of compiler
data IState = IState
  { _multiline :: !(Maybe T.Text)
   -- ^ When multiline mode is enable the interpreter storage all code
   --   until found a multiline close token. Then load text to compiler.
  , _memory    :: !World
  -- , _docs :: Docs -- Map Text DocFormatted
  }

makeLenses ''IState
