{-# LANGUAGE TemplateHaskell #-}
module Compiler.Scope.Types where

import           Control.Monad.State.Strict
import           Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Text as T
import           Lens.Micro.Platform

data ScopeError = NoIdFound T.Text
  deriving Show

type ScopeM last = ExceptT ScopeError (State (Scope last))

data Scope last = Scope
  { _nextId :: Word
  , _currentScope :: ScopeInfo last
  , _stackScope :: [ScopeInfo last]
  }

data ScopeInfo last = ScopeInfo
  { _prevInfo :: last
  , _renameInfo :: M.Map T.Text Word
  }

makeLenses ''Scope
makeLenses ''ScopeInfo

