module Compiler.Object.Types where

import qualified Data.Text                  as T
import qualified Data.Map                   as M
import           Control.Monad.Trans.Free

import Compiler.World.Types
import {-# SOURCE #-} Compiler.Instruction.Types


class ToObject o where
  toObject :: o -> Object

class FromObject o where
  fromObject :: Object -> o

data Object
  = OStr T.Text
  | OBool Bool
  | ODouble Double
  | ONum Int
  | ORegex T.Text -- TODO: search precompiled type
  | OShellCommand T.Text
  | OFunc (M.Map T.Text Object) [Word] (FreeT Instruction StWorld VarAccessor)
  | ONative ([Object] -> FreeT Instruction StWorld VarAccessor)
  | ONone

instance Show Object where
  show (OStr text) = show text
  show (OBool bool) = show bool
  show (ODouble double) = show double
  show (ORegex reg) = "regex/" ++ show reg ++ "/"
  show (OShellCommand cmd) = "cmd$" ++ show cmd ++ "$"
  show (ONum num) = show num
  show (OFunc _ _ _) = "[Function]"
  show (ONative _) = "[Native Function]"
  show ONone = "None"

instance ToObject Object where
  toObject = id

instance FromObject Object where
  fromObject = id

instance ToObject Bool where
  toObject bool = OBool bool

instance FromObject Bool where
  fromObject (OBool bool) = bool
  fromObject _ = error "Not Implemented"


newtype ObjectError = ObjectError { info :: T.Text }





