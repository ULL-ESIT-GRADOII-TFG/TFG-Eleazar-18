module Compiler.Interpreter.Types where

import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Except

import Compiler.World.Types
import {-# SOURCE #-} Compiler.Instruction.Types
import Compiler.Object.Types

data CompilerError

type Prog = Except CompilerError (FreeT Instruction StWorld Object)
