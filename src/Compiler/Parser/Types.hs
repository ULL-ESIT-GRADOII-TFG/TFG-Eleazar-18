{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Parser.Types where

import           Data.Default
import           Data.Text.Prettyprint.Doc

-- import           Compiler.Prettify
import           Compiler.Utils

type Loc = TokenInfo

data TokenInfo = TokenInfo
  { _start :: !SrcPos
  , _end   :: !SrcPos
  }
  deriving Show

data SrcPos = SrcPos
  { _column :: !Int
  , _row    :: !Int
  , _offset :: !Int
  }
  deriving Show

instance Default SrcPos where
  def = SrcPos 0 0 0

instance Default TokenInfo where
  def = TokenInfo def def

instance Pretty SrcPos where
  pretty (SrcPos col row _offset) =
    "L" <>
    pretty row <>
    ":C" <>
    pretty col
    -- text " >> " <>
    -- text (show offset)

instance Pretty TokenInfo where
  pretty (TokenInfo st en) =
    "Tok" <+>
    pretty st <+>
    "- to -" <+>
    pretty en

dummyTokenInfo :: TokenInfo
dummyTokenInfo = def

makeSuffixLenses ''TokenInfo
makeSuffixLenses ''SrcPos
