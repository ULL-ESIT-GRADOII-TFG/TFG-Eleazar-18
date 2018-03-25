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
  | OFunc (M.Map T.Text Object) [Word] (FreeT Instruction StWorld VarAccessor)
  | ONone

instance Show Object where
  show (OStr text) = show text
  show (OBool bool) = show bool
  show (ODouble double) = show double
  show (ONum num) = show num
  show (OFunc _ _ _) = "[Function]"
  show ONone = "None"

newtype ObjectError = ObjectError { info :: T.Text }

