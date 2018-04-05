{-# LANGUAGE TemplateHaskell #-}
module Compiler.World.Types where

import           Control.Monad.Trans.Free
import           Control.Monad.State.Strict
import qualified Data.Map                as M
import qualified Data.IntMap                as IM
import qualified Data.Text as T
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Object.Types
import {-# SOURCE #-} Compiler.Instruction.Types


data ClassDefinition = ClassDefinition
  { _name :: T.Text
  --, _values :: M.Map T.Text ([Object] -> FreeT Instruction StWorld Object)
  }
  deriving Show

-- | Used to storage vars into memory, atleast its reference structure
data Var
  = Var
    { _refs :: !Word
    , _rawVar :: !Object
    }
  | SetVar
    { _refsMain :: !Word
    , _vars :: IM.IntMap Var
    , _rawMainVar :: !Object
    }
  deriving Show

data World = World
  { _table :: IM.IntMap Var
  , _typeDefinitions :: IM.IntMap ClassDefinition
  }
  deriving Show

-- TODO: Error
type StWorld = StateT World IO


makeLenses ''Var
makeLenses ''World
