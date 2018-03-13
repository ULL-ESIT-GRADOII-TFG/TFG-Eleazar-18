module Compiler.Object.Types where

import qualified Data.Text                  as T
import qualified Data.Map                   as M
import           Control.Monad.Trans.Free

import Compiler.World.Types
import {-# SOURCE #-} Compiler.Instruction.Types

data Object
  = OStr T.Text
  | OBool Bool
  | ODouble Double
  | ONum Int
  | OFunc (M.Map T.Text Object) [T.Text] (FreeT Instruction StWorld VarAccessor)
  | ONone

instance Show Object where
  show _ = "TODO"

newtype ObjectError = ObjectError { info :: T.Text }

