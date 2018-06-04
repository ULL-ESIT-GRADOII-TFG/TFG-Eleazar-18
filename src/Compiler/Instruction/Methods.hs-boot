{-# LANGUAGE FlexibleContexts #-}
module Compiler.Instruction.Methods where

import           Control.Monad.Trans.Free

import           Compiler.Types

runProgram :: FreeT Instruction StWorld Object -> StWorld Object

(=:) :: (MonadFree Instruction m) => AddressRef -> Object -> m Object

getVal :: (MonadFree Instruction m) => AddressRef -> m Object
