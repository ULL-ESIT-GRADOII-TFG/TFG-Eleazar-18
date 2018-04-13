{-# LANGUAGE TemplateHaskell #-}
module Compiler.Scope.Types where

import           Control.Monad.State.Strict
import           Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Text as T
import           Lens.Micro.Platform

import Compiler.Instruction.Types


data ScopeError
  = NoIdFound T.Text
  | InternalFail
  deriving Show

type ScopeM = ExceptT ScopeError (State Scope)

data Scope = Scope
  { _nextId :: Word
  , _currentScope :: ScopeInfo
  , _stackScope :: [ScopeInfo]
  } deriving Show

newtype ScopeInfo = ScopeInfo
  { _renameInfo :: M.Map T.Text AddressRef
  } deriving Show


makeLenses ''Scope
makeLenses ''ScopeInfo
