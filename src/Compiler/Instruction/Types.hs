{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE TemplateHaskell           #-}
module Compiler.Instruction.Types where

import           Control.Monad.Trans.Free
import           Control.Monad.State.Strict
import           Lens.Micro.Platform
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Compiler.Object.Types
import {-# SOURCE #-} Compiler.World.Types


type Prog = FreeT Instruction StWorld Object

data AddressRef = AddressRef
  { ref :: Word
  , dynPath :: [T.Text]
  }
  deriving Show

simple :: Word -> AddressRef
simple word = AddressRef word []

-- | Intermediate set of instructions.
--
-- TODO: Extract Ref var from ADT
data Instruction next
  = CallCommand !AddressRef ![Object] (Object -> next)
  -- ^ Make a call to and defined function
  | Assign !AddressRef !Object next
  -- ^ Assign an object to local variable
  | DropVar !AddressRef next
  | GetVal !AddressRef (Object -> next)
  | Loop !Object (Object -> Prog) next
  | Cond !Object
      Prog
      Prog
      (Object -> next)
  | End
  -- ^ End program, ignore all after that
  deriving Functor


-- | Auxiliar type to debug Instruction Set
data PPrint = PPrint
  { _fakeId :: !Word
  , _level :: !Word
  , _generate :: !LT.Text
  }
  deriving Show

type StPrint = StateT PPrint IO


makeLenses ''PPrint
