{-# LANGUAGE FlexibleInstances #-}
module Compiler.Prelude.Types where

import           Control.Monad.Trans.Free

import Compiler.Object.Types
import Compiler.Scope.Types
import Compiler.Scope.Methods
import Compiler.World.Types
import Compiler.Instruction.Types

class Normalize a where
  normalize :: a -> [Object] -> FreeT Instruction StWorld VarAccessor

instance Normalize (FreeT Instruction StWorld VarAccessor)  where
  normalize f ls =
    case ls of
      [] -> f
      _ -> return $ Raw ONone -- TODO: Add Error

instance Normalize r => Normalize (Object -> r) where
  normalize _   []     = return $ Raw ONone -- TODO: Add Error
  normalize fun (a:xs) = normalize (fun a) xs
