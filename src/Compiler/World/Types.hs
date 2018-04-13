{-# LANGUAGE TemplateHaskell #-}
module Compiler.World.Types where

import           Control.Monad.State.Strict
import qualified Data.Map                as M
import qualified Data.IntMap                as IM
import qualified Data.Text as T
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Object.Types
--import {-# SOURCE #-} Compiler.Instruction.Types
import Compiler.Scope.Types


data ClassDefinition = ClassDefinition
  { _name :: T.Text
  , _values :: M.Map T.Text Word
  }
  deriving Show

-- | Used to storage vars into memory, atleast its reference structure
data Var = Var
  { _refs :: !Word
  , _rawVar :: !Object
  }
  deriving Show

data World = World
  { _table :: IM.IntMap Var
  , _typeDefinitions :: IM.IntMap ClassDefinition
  , _scope :: Scope
  }

instance Show World where
  show _ = "TODO"

-- TODO: Error
type StWorld = StateT World IO


makeLenses ''Var
makeLenses ''World
makeLenses ''ClassDefinition
