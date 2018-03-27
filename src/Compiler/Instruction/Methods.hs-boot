module Compiler.Instruction.Methods where

import           Control.Monad.Trans.Free

import Compiler.Instruction.Types
import Compiler.World.Types

runProgram :: FreeT Instruction StWorld VarAccessor -> StWorld VarAccessor
