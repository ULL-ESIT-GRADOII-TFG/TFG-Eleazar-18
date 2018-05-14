{-# LANGUAGE TemplateHaskell #-}
module Compiler.Instruction.Types where

import           Control.Monad.State.Strict
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Lens.Micro.Platform


data AddressRef = AddressRef
  { _ref     :: Word
  , _dynPath :: [T.Text]
  }
  deriving Show

simple :: Word -> AddressRef
simple word = AddressRef word []

-- | Auxiliar type to debug Instruction Set
data PPrint = PPrint
  { _fakeId             :: !Word
  , _currentIndentLevel :: !Word
  , _generate           :: !LT.Text
  }
  deriving Show

type StPrint = StateT PPrint IO


makeLenses ''AddressRef
makeLenses ''PPrint
