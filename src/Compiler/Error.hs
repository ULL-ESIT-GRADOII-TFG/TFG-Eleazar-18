{-# LANGUAGE FlexibleContexts #-}
module Compiler.Error where

import           Control.Monad.Except
import           Control.Monad.Trans.Free
import qualified Data.Text                as T
import           Lens.Micro.Platform
import           Text.PrettyPrint

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
  --getAll :: a -> (ErrorLevel, String)

  getMessage :: a -> String
  getLevel :: a -> ErrorLevel
  getLevel _ = Error

instance ReadeableError a => ReadeableError (ErrorInfo a) where
  getMessage = getMessage . _errorInternal
  getLevel = getLevel . _errorInternal

throw :: (GetInfo m, MonadError (ErrorInfo a) m, ReadeableError a) => a -> m b
throw err = do
  info <- getInfo
  throwError (ErrorInfo info err)

-- | Render a error adding location of code (line) and file name
renderErrorWithSource :: ReadeableError a => ErrorInfo a -> T.Text -> String -> Doc
renderErrorWithSource err@(ErrorInfo tok _) source filename = renderError err
  <> nest 2 codeFormatted
  <> text "File: " <> text filename
  where
    errorLines = 1 + (tok^.endA.rowA) - (tok^.startA.rowA)
    getSourceCode = take errorLines $ drop (tok^.startA.rowA) $ T.lines source
    codeFormatted =
      vcat $ zipWith (\num code ->
        text (show num) <> text " " <> text (T.unpack code))
        [tok^.startA.rowA..] getSourceCode


renderError :: ReadeableError a => ErrorInfo a -> Doc
renderError (ErrorInfo _tok internal) =
  text (show $ getLevel internal) <> text ": " <> text (getMessage internal)
