{-# LANGUAGE OverloadedStrings #-}
module Compiler.World.Types where

import qualified Data.Text            as T

import           Compiler.Error
import           Compiler.Scope.Types


data WorldError
  = NotFoundObject Word
  | NotPropertyFound { initial :: [T.Text], propertyNotFound :: T.Text, last :: [T.Text] }
  -- ^ Object access initial accessor, failed access identifier, everything else
  | NotIterable String
  | NotBoolean String
  | NotCallable String
  | NumArgsMissmatch Int Int
  -- ^ Expected and given
  | NotImplicitConversion
  | ExcededRecursiveLimit
  | DropVariableAlreadyDropped
  | NotExtensibleObject
  | WorldError T.Text
  | ScopeError (ErrorInfo ScopeError)
  deriving Show

instance ReadeableError WorldError where
  getAll err = case err of
    NotFoundObject ref -> (,) Error $
      "It wasn't found ref `" ++ show ref ++ "` in memory."
    NotPropertyFound ini mid lst -> (,) Error $
      "Can't be accessed `" ++ T.unpack mid ++"` property in `"
      ++ T.unpack (T.intercalate "." (ini ++ [mid] ++ lst)) ++ "`"
    NotIterable ty               -> (,) Error $
      "No iterable object `" ++ ty ++ "` it should implement __map__"
    NotCallable ty                -> (,) Error $
      "No callable object `" ++ ty ++ "` it should implement __call__"
    NotBoolean ty                -> (,) Error $
      "No a boolean object `" ++ ty ++ "` it should implement __bool__"
    NumArgsMissmatch expected given -> (,) Error $
      "It was expected to get " ++ show expected ++ " args given " ++ show given
    NotImplicitConversion      -> (,) Error $
      "No implicit conversion from `type`  to `type`"
    ExcededRecursiveLimit      -> (,) Critical $
      "Exceded recursion limit with internal references"
    DropVariableAlreadyDropped -> (,) Critical $
      "Variable already dropped"
    NotExtensibleObject        -> (,) Error $
      "Only objects and dictionaries can be expanded, trying to expand `TODO`"
    WorldError txt            -> (,) Error $
      (T.unpack txt)
    ScopeError err'            -> getAll err'
