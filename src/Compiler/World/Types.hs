{-# LANGUAGE TemplateHaskell #-}
module Compiler.World.Types where

import           Control.Monad.State.Strict
import qualified Data.IntMap                as IM
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Object.Types

data Var = Var
  { _refs :: !Word
  , _rawVar :: !Object
  }

data World = World
  { _table :: IM.IntMap Var
  }

-- TODO: Error
type StWorld = StateT World IO


makeLenses ''Var
makeLenses ''World
