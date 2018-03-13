{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE TemplateHaskell           #-}
module Compiler.Instruction.Types where

import           Control.Monad.Trans.Free
import           Control.Monad.State.Strict
import           Lens.Micro.Platform
import qualified Data.Text.Lazy as T

import Compiler.Object.Types
import Compiler.World.Types

data VarAccessor
  = Ref Word
  | Raw Object
  deriving Show

data Instruction next
  = CallCommand !Word ![VarAccessor] (VarAccessor -> next)
  -- ^ Make a call to and defined function
  | Assign !Word !VarAccessor next
  -- ^ Assign an object to local variable
  | DropVar !Word next
  | GetVal !Word (VarAccessor -> next)
  | Loop VarAccessor (VarAccessor -> FreeT Instruction StWorld VarAccessor) next
  | Cond VarAccessor
      (FreeT Instruction StWorld VarAccessor)
      (FreeT Instruction StWorld VarAccessor)
      (VarAccessor -> next)
  | End
  -- ^ End program, ignore all after that
  deriving Functor


data PPrint = PPrint
  { _fakeId :: !Word
  , _level :: !Word
  , _generate :: !T.Text
  }
  deriving Show

type StPrint = StateT PPrint IO


makeLenses ''PPrint

