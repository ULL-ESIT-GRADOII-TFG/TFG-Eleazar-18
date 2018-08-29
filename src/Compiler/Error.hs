{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Error where

import           Control.Monad.Except
import           Control.Monad.Trans.Free
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Lens.Micro.Platform

import           Compiler.Parser.Types
import           Compiler.Utils


data ErrorLevel = Critical | Warning | Error deriving (Show, Eq)

data ErrorInfo a = ErrorInfo
  { _errorInfo     :: TokenInfo
  , _errorInternal :: a
  } deriving Show

makeSuffixLenses ''ErrorInfo

data ScopeError
  = NotDefinedObject T.Text
  | InternalFail
  | ErrorClass
  | NoSavedPathVar
  deriving Show

instance ReadeableError ScopeError where
  getAll err = case err of
    NotDefinedObject name -> (,) Error $
      "It wasn't found `" <> name <> "` in the current scope."
    InternalFail      -> (,) Critical
      "A Internal Fail into Scope phase was happened. Report it."
    ErrorClass        -> (,) Error
      ""
    NoSavedPathVar -> (,) Critical
      "Problem generating unique ids for variable name. Report it."

data WorldError
  = NotFoundObject Int
  | NotPropertyFound { initialLeftover :: [T.Text], propertyNotFound :: T.Text, finalLeftover :: [T.Text] }
  -- ^ Object access initial accessor, failed access identifier, everything else
  | NotIterable T.Text
  | NotBoolean T.Text
  | NotCallable T.Text
  | NumArgsMissmatch Int Int
  -- ^ Expected and given
  | NotImplicitConversion T.Text T.Text
  | ExcededRecursiveLimit
  | DropVariableAlreadyDropped
  | NotExtensibleObject
  | WorldError T.Text
  | ScopeError ScopeError
  deriving Show

instance ReadeableError WorldError where
  getAll err = case err of
    NotFoundObject ref -> (,) Error $
      "It wasn't found ref `" <> T.pack (show ref) <> "` in memory."
    NotPropertyFound ini mid lst -> (,) Error $
      "Can't be accessed `" <> mid <> " property in `"
      <> (T.intercalate "." (ini ++ [mid] ++ lst)) <> "`"
    NotIterable ty               -> (,) Error $
      "No iterable object `" <> ty <> "` it should implement __map__"
    NotCallable ty                -> (,) Error $
      "No callable object `" <> ty <> "` it should implement __call__"
    NotBoolean ty                -> (,) Error $
      "No a boolean object `" <> ty <> "` it should implement __bool__"
    NumArgsMissmatch expected given -> (,) Error $
      "It was expected to get " <> T.pack (show expected) <> " args given " <> T.pack (show given)
    NotImplicitConversion fromTy toTy -> (,) Error $
      "No implicit conversion from `" <> fromTy <> "`  to `" <> toTy <> "`"
    ExcededRecursiveLimit      -> (,) Critical $
      "Exceded recursion limit with internal references"
    DropVariableAlreadyDropped -> (,) Critical $
      "Variable already dropped"
    NotExtensibleObject        -> (,) Error $
      "Only objects and dictionaries can be expanded, trying to expand `TODO`"
    WorldError txt            -> (,) Error txt
    ScopeError err'            -> getAll err'

-- | Get TokenInfo, used to generate errors see `throw`
class GetInfo m where
  getInfo :: m TokenInfo

instance (GetInfo m, Monad m) => GetInfo (ExceptT a m) where
  getInfo = lift getInfo

instance (GetInfo m, Monad m) => GetInfo (FreeT i m) where
  getInfo = lift getInfo

class ReadeableError a where
  getAll :: a -> (ErrorLevel, T.Text)
  getAll = (,) <$> getLevel <*> getMessage

  getMessage :: a -> T.Text
  getMessage = snd . getAll
  getLevel :: a -> ErrorLevel
  getLevel = fst . getAll
  {-# MINIMAL getAll | getMessage, getLevel #-}

instance ReadeableError a => ReadeableError (ErrorInfo a) where
  getMessage = getMessage . _errorInternal
  getLevel = getLevel . _errorInternal

throw :: (GetInfo m, MonadError (ErrorInfo a) m, ReadeableError a) => a -> m b
throw err = do
  info <- getInfo
  throwError (ErrorInfo info err)

throwWithInfo :: MonadError (ErrorInfo a) m => TokenInfo -> a -> m b
throwWithInfo info err = throwError (ErrorInfo info err)

-- | Render a error adding location of code (line) and file name
renderErrorWithSource :: ReadeableError a => ErrorInfo a -> T.Text -> T.Text -> Doc ()
renderErrorWithSource err@(ErrorInfo tok _) source filename =
  renderError err
  <> "\n\n"
  <> nest 2 codeFormatted
  <> "\n\n"
  <> "File: " <> pretty filename
  where
    errorLines = 1 + (tok^.endA.rowA) - (tok^.startA.rowA)
    getSourceCode = take errorLines $ drop (tok^.startA.rowA) $ T.lines source
    codeFormatted =
      vcat $ zipWith (\num code ->
        pretty num <+> pretty code)
        [tok^.startA.rowA..] getSourceCode

-- | Render an error without location information. Useful for interpreter.
renderError :: ReadeableError a => ErrorInfo a -> Doc ()
renderError (ErrorInfo _tok internal) =
  (pretty . show $ getLevel internal) <> ":" <+> pretty (getMessage internal)
