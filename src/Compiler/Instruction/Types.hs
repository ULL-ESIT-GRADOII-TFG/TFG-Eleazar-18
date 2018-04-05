{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE TemplateHaskell           #-}
module Compiler.Instruction.Types where

import           Control.Monad.Trans.Free
import           Control.Monad.State.Strict
import           Lens.Micro.Platform
import qualified Data.Text.Lazy as T

import Compiler.Object.Types
import Compiler.World.Types

data Instruction next
  = CallCommand ![Word] ![Object] (Object -> next)
  -- ^ Make a call to and defined function
  | Assign ![Word] !Object next
  -- ^ Assign an object to local variable
  | DropVar ![Word] next
  | GetVal ![Word] (Object -> next)
  | Loop Object (Object -> FreeT Instruction StWorld Object) next
  | Cond Object
      (FreeT Instruction StWorld Object)
      (FreeT Instruction StWorld Object)
      (Object -> next)
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
