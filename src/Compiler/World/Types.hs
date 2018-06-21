{-# LANGUAGE OverloadedStrings #-}
module Compiler.World.Types where

import qualified Data.Text            as T

import           Compiler.Error
import           Compiler.Scope.Types


data WorldError
  = NotFoundObject Word
  | NotPropertyFound { initial :: [T.Text], propertyNotFound :: T.Text, last :: [T.Text] }
  -- ^ Object access initial accessor, failed access identifier, everything else
  | NotIterable
  | NotCallable
  | NumArgsMissmatch
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
    NotPropertyFound ini err lst -> (,) Error $
      "Can't be accessed `" ++ T.unpack err ++"` property in `"
      ++ T.unpack (T.intercalate "." (ini ++ [err] ++ lst)) ++ "`"
    NotIterable                -> (,) Error $
      "No iterable object `TODO` it should implement __iter__"
    NotCallable                -> (,) Error $
      "No callable object `TODO` it should implement __call__"
    NumArgsMissmatch           -> (,) Error $
      "It was expected to get x args given xx"
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
