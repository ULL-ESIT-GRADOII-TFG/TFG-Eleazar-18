{-# LANGUAGE TemplateHaskell #-}
module Compiler.Scope.Types where

import           Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Text as T
import           Lens.Micro.Platform

type ScopeM last = State (Scope last)

data Scope last = Scope
  { _nextScope :: Word
  , _lastScopeInfo :: Maybe (ScopeInfo last)
  }

data ScopeInfo last = ScopeInfo
  { _prevInfo :: last
  , _lastScope :: Maybe (ScopeInfo last)
  , _renameInfo :: M.Map T.Text Word
  }

makeLenses ''Scope
makeLenses ''ScopeInfo

