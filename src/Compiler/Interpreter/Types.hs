{-# LANGUAGE TemplateHaskell           #-}
module Compiler.Interpreter.Types where

import           Control.Monad.State.Strict
import qualified Data.Text                  as T
import           Control.Monad.Trans.Free
import           System.Console.Haskeline   (InputT)
import           Control.Monad.Trans.Except
import           Lens.Micro.Platform

import Compiler.World.Types
import Compiler.Instruction.Types
import Compiler.Object.Types

type Prog = Except CompilerError (FreeT Instruction StWorld Object)

-- TODO: Define better
data CompilerError
  = Lexer String
  | Parser String
  | Scope String

-- | Used to control flow of all interpreter
type Interpreter = StateT IState (InputT IO)

-- | Represent internal state of compiler
data IState = IState
  { _multiline :: !(Maybe T.Text)
   -- ^ When multiline mode is enable the interpreter storage all code
   --   until found a multiline close token. Then load text to compiler.
  , _memory :: !World
  }

makeLenses ''IState
