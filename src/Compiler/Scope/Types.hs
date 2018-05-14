{-# LANGUAGE TemplateHaskell #-}
module Compiler.Scope.Types where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import           Compiler.Instruction.Types


data ScopeError
  = NoIdFound T.Text
  | InternalFail
  deriving Show

type ScopeM = ExceptT ScopeError (State Scope)

data Scope = Scope
  { _nextId          :: Word
  , _currentScope    :: ScopeInfo
  , _stackScope      :: [ScopeInfo]
  , _typeDefinitions :: IM.IntMap ClassDefinition
  } deriving Show

instance Default Scope where
  def = Scope
    { _nextId       = 0
    , _currentScope = ScopeInfo {_renameInfo = mempty}
    , _stackScope   = []
    , _typeDefinitions = mempty
    }

newtype ScopeInfo = ScopeInfo
  { _renameInfo :: M.Map T.Text AddressRef
  } deriving Show

data ClassDefinition = ClassDefinition
  { _nameClass       :: T.Text
  , _attributesClass :: M.Map T.Text Word
  }
  deriving Show


makeLenses ''Scope
makeLenses ''ScopeInfo
makeLenses ''ClassDefinition
