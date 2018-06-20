{-# LANGUAGE FlexibleInstances #-}
module Compiler.Prelude.Types where

import           Control.Monad.Except
import           Control.Monad.Trans.Free

import           Compiler.Error
import           Compiler.Object.Types
import           Compiler.Types


data Assoc = LeftAssoc | RightAssoc deriving (Show, Eq)

class Normalize a where
  normalize :: a -> [Object] -> FreeT Instruction StWorld Object

instance Normalize (FreeT Instruction StWorld Object)  where
  normalize f ls =
    case ls of
      [] -> f
      _  -> throw NumArgsMissmatch

instance Normalize Object where
  normalize f ls =
    case ls of
      [] -> return f
      _  -> throw NumArgsMissmatch

instance (Normalize r, FromObject a) => Normalize (a -> r) where
  normalize _   []     = throw NumArgsMissmatch
  normalize fun (a:xs) = do
    obj <- lift $ fromObject a
    normalize (fun obj) xs
