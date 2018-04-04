{-# LANGUAGE TemplateHaskell #-}
module Compiler.World.Types where

import           Control.Monad.Trans.Free
import           Control.Monad.State.Strict
import qualified Data.Map                as M
import qualified Data.IntMap                as IM
import qualified Data.Text as T
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Object.Types
import {-# SOURCE #-}Compiler.Instruction.Types


data ClassDefinition = ClassDefinition
  { _name :: T.Text
  , _values :: M.Map T.Text ([Object] -> FreeT Instruction StWorld VarAccessor)
  }

data Var = Var
  { _refs :: !Word
  , _rawVar :: !Object
  }

data World = World
  { _table :: IM.IntMap Var
  , _typeDefinitions :: IM.IntMap ClassDefinition
  }

-- TODO: Error
type StWorld = StateT World IO


makeLenses ''Var
makeLenses ''World
