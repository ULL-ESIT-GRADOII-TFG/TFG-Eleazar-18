{-# LANGUAGE TemplateHaskell #-}
module Compiler.World.Types where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Object.Types
--import {-# SOURCE #-} Compiler.Instruction.Types
import           Compiler.Scope.Types


data ClassDefinition = ClassDefinition
  { _name   :: T.Text
  , _values :: M.Map T.Text Word
  }
  deriving Show

-- | Used to storage vars into memory, atleast its reference structure
data Var = Var
  { _refs   :: !Word
  , _rawVar :: !Object
  }
  deriving Show

data World = World
  { _table           :: IM.IntMap Var
  , _typeDefinitions :: IM.IntMap ClassDefinition
  , _scope           :: Scope
  }

instance Show World where
  show _ = "TODO"

data WorldError
  = NoFoundVar
  deriving Show

-- | Note: `ExceptT` wraps monad state, in case of fail discard memory. That it is the predicate to follow.
type StWorld = StateT World (ExceptT WorldError IO)


makeLenses ''Var
makeLenses ''World
makeLenses ''ClassDefinition
