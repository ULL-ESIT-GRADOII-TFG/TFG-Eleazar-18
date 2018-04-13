module Compiler.World.Types where

import           Control.Monad.State.Strict


data ClassDefinition

data Var

data World

type StWorld = StateT World IO
