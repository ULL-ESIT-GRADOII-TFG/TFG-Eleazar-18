{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Parser.Types where

import           Data.Default
import           Data.Text.Prettyprint.Doc

import           Compiler.Prettify
import           Compiler.Utils


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

instance Prettify SrcPos where
  prettify (SrcPos col row _offset) _ =
    "L" <>
    pretty row <>
    ":C" <>
    pretty col
    -- text " >> " <>
    -- text (show offset)

instance Prettify TokenInfo where
  prettify (TokenInfo st en) verbose =
    "TknInfo start" <+>
    prettify st verbose <+>
    "end" <+>
    prettify en verbose

dummyTokenInfo :: TokenInfo
dummyTokenInfo = def

makeSuffixLenses ''TokenInfo
makeSuffixLenses ''SrcPos
