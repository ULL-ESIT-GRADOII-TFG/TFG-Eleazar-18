module Compiler.Instruction.Types where

import           Control.Monad.Trans.Free

import {-# SOURCE #-} Compiler.Object.Types
import {-# SOURCE #-} Compiler.World.Types

type Prog = FreeT Instruction StWorld Object

data AddressRef

data Instruction next
