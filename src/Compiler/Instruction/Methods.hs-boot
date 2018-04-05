module Compiler.Instruction.Methods where

import           Control.Monad.Trans.Free

import Compiler.Instruction.Types
import Compiler.World.Types
import Compiler.Object.Types

runProgram :: FreeT Instruction StWorld Object -> StWorld Object
