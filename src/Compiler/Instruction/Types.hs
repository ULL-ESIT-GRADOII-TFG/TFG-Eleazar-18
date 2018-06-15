{-# LANGUAGE TemplateHaskell #-}
module Compiler.Instruction.Types where

import           Control.Monad.State.Strict
import           Data.List
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Text.PrettyPrint

import           Compiler.Prettify
import           Compiler.Utils


data AddressRef = AddressRef
  { _ref     :: Word
  , _dynPath :: [T.Text]
  }
  deriving Show

simple :: Word -> AddressRef
simple word = AddressRef word []

instance Prettify AddressRef where
  prettify (AddressRef r p) _verbose =
    text "ADDR#" <> text (show r) <> text "->" <> (text $ intercalate "." $ map T.unpack p)

-- | Auxiliar type to debug Instruction Set
data PPrint = PPrint
  { _fakeId             :: !Word
  , _currentIndentLevel :: !Word
  , _generate           :: !LT.Text
  }
  deriving Show

type StPrint = StateT PPrint IO


makeSuffixLenses ''AddressRef
makeSuffixLenses ''PPrint
