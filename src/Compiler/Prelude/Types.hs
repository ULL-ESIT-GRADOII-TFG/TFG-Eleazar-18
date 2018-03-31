{-# LANGUAGE FlexibleInstances #-}
module Compiler.Prelude.Types where

import           Control.Monad.Trans.Free

import Compiler.Object.Types
import Compiler.World.Types
import Compiler.Instruction.Types


data Assoc = LeftAssoc | RightAssoc deriving (Show, Eq)

class Normalize a where
  normalize :: a -> [Object] -> FreeT Instruction StWorld VarAccessor

instance Normalize (FreeT Instruction StWorld VarAccessor)  where
  normalize f ls =
    case ls of
      [] -> f
      _ -> return $ Raw ONone -- TODO: Add Error

instance Normalize Object where
  normalize f ls =
    case ls of
      [] -> return $ Raw f
      _ -> return $ Raw ONone -- TODO: Add Error

-- TODO: cambiar object por algo convertible a el -> normalize (+)
instance (Normalize r, FromObject a) => Normalize (a -> r) where
  normalize _   []     = return $ Raw ONone -- TODO: Add Error
  normalize fun (a:xs) = normalize (fun (fromObject a)) xs
