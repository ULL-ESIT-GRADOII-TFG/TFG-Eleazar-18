{-# LANGUAGE TemplateHaskell #-}
module Compiler.Parser.Types where

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

dummyTokenInfo :: TokenInfo
dummyTokenInfo = TokenInfo
  { _start = SrcPos
    { _column = 0
    , _row    = 0
    , _offset = 0
    }
  , _end   = SrcPos
    { _column = 0
    , _row    = 0
    , _offset = 0
    }
  }

makeLenses ''SrcPos
