{-# LANGUAGE TemplateHaskell #-}
module Compiler.Parser.Types where

import Data.Default
import           Lens.Micro.Platform

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

dummyTokenInfo :: TokenInfo
dummyTokenInfo = def

makeLenses ''SrcPos
