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
  | OFunc (M.Map T.Text Object) [Word] (FreeT Instruction StWorld Object)
  | OObject Word (M.Map T.Text Object)
  | ONative ([Object] -> FreeT Instruction StWorld Object)
  | ORef [Word]
  | ONone

instance Show Object where
  show OStr{} = "[String]"
  show OBool{} = "[Bool]"
  show ODouble{} = "[Double]"
  show ORegex{} = "[Regex]"
  show OShellCommand{} = "[ShellCmd]"
  show ONum{} = "[Int]"
  show OFunc{} = "[Function]"
  show ONative{} = "[Native Function]"
  show ORef{} = "[Reference]"
  show OObject{} = "[Object]"
  show ONone = "[None]"

instance ToObject Object where
  toObject = id

instance FromObject Object where
  fromObject = id

instance ToObject Bool where
  toObject = OBool

instance FromObject Bool where
  fromObject (OBool bool) = bool
  fromObject _ = error "Not Implemented"

instance ToObject Int where
  toObject = ONum

instance FromObject Int where
  fromObject (ONum num) = num
  fromObject _ = error "Not Implemented"

newtype ObjectError = ObjectError { info :: T.Text }
