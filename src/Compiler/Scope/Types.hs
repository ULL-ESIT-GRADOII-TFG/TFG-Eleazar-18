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
import Compiler.Parser.Types


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

instance Default ScopeInfo where
  def = ScopeInfo mempty

data ClassDefinition = ClassDefinition
  { _nameClass       :: T.Text
  , _attributesClass :: M.Map T.Text Word
  }
  deriving Show

data ScopeInfoAST = ScopeInfoAST
  { _tokenInfo :: TokenInfo
  , _scopeInfo :: ScopeInfo
  } deriving Show

instance Default ScopeInfoAST where
  def = ScopeInfoAST def def


makeLenses ''Scope
makeLenses ''ScopeInfo
makeLenses ''ScopeInfoAST
makeLenses ''ClassDefinition
