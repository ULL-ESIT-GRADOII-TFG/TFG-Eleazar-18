{-# LANGUAGE TemplateHaskell #-}
module Compiler.Parser.Types where

import           Data.Default
import           Lens.Micro.Platform
import           Text.PrettyPrint

import           Compiler.Prettify


data TokenInfo = TokenInfo
  { _start :: SrcPos
  , _end   :: SrcPos
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
  prettify (SrcPos col row offset) _ =
    text "L" <>
    text (show row) <>
    text ":C" <>
    text (show col) <>
    text " >> " <>
    text (show offset)

instance Prettify TokenInfo where
  prettify (TokenInfo st en) verbose =
    text "TknInfo start " <>
    prettify st verbose <>
    text " end " <>
    prettify en verbose

dummyTokenInfo :: TokenInfo
dummyTokenInfo = def

makeLenses ''SrcPos
