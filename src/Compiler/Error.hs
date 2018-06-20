{-# LANGUAGE FlexibleContexts #-}
module Compiler.Error where

import           Control.Monad.Except
import           Control.Monad.Trans.Free
import qualified Data.Text                as T

import           Compiler.Parser.Types


data ErrorLevel = Critical | Warning | Error deriving (Show, Eq)

data ErrorInfo a = ErrorInfo
  { _errorInfo     :: TokenInfo
  , _errorInternal :: a
  } deriving Show

-- | Get TokenInfo, used to generate errors see `throw`
class GetInfo m where
  getInfo :: m TokenInfo

instance (GetInfo m, Monad m) => GetInfo (ExceptT a m) where
  getInfo = lift getInfo

instance (GetInfo m, Monad m) => GetInfo (FreeT i m) where
  getInfo = lift getInfo

class ReadeableError a where
  getMessage :: a -> T.Text
  getLevel :: a -> ErrorLevel
  getLevel _ = Error

instance ReadeableError a => ReadeableError (ErrorInfo a) where
  getMessage = getMessage . _errorInternal
  getLevel = getLevel . _errorInternal

throw :: (GetInfo m, MonadError (ErrorInfo a) m, ReadeableError a) => a -> m b
throw err = do
  info <- getInfo
  throwError (ErrorInfo info err)
