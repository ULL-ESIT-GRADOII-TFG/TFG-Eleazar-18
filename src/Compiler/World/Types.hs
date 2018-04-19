{-# LANGUAGE TemplateHaskell #-}
module Compiler.World.Types where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.IntMap                as IM
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Object.Types
--import {-# SOURCE #-} Compiler.Instruction.Types
import           Compiler.Scope.Types


-- | Used to storage vars into memory, atleast its reference structure
data Var = Var
  { _refCounter :: !Word
  , _rawObj     :: !Object
  }
  deriving Show

data World = World
  { _table :: IM.IntMap Var
  , _scope :: Scope
  }
  deriving Show

data WorldError
  = NoFoundVar
  deriving Show

-- | Note: `ExceptT` wraps monad state, in case of fail discard memory. That it is the predicate to follow.
type StWorld = StateT World (ExceptT WorldError IO)


makeLenses ''Var
makeLenses ''World
