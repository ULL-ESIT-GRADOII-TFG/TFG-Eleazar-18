{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
module Compiler.Instruction.Types where

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Free
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Lens.Micro.Platform

import           Compiler.Object.Types
import {-# SOURCE #-} Compiler.World.Types


type Prog = FreeT Instruction StWorld Object

data AddressRef = AddressRef
  { _ref     :: Word
  , _dynPath :: [T.Text]
  }
  deriving Show

simple :: Word -> AddressRef
simple word = AddressRef word []

-- | Intermediate set of instructions.
data Instruction next
  = CallCommand !AddressRef ![Object] (Object -> next)
  -- ^ Make a call to and defined function
  | Assign !AddressRef !Object next
  -- ^ Assign an object to local variable
  | DropVar !AddressRef next
  -- ^ Remove a var from memory
  | GetVal !AddressRef (Object -> next)
  -- ^ Retrieve a object from a memory reference
  | Loop !Object (Object -> Prog) next
  -- ^ Loop over a object
  | Cond !Object
      Prog
      Prog
      (Object -> next)
  -- ^ If sentence given a object
  | End
  -- ^ End program, ignore all after that
  deriving Functor


-- | Auxiliar type to debug Instruction Set
data PPrint = PPrint
  { _fakeId   :: !Word
  , _level    :: !Word
  , _generate :: !LT.Text
  }
  deriving Show

type StPrint = StateT PPrint IO


makeLenses ''AddressRef
makeLenses ''PPrint
