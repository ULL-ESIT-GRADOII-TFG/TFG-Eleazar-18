module Compiler.Object.Types where

import qualified Data.Map                   as M
import qualified Data.Vector                as V
import qualified Data.Text                  as T

import {-# SOURCE #-} Compiler.Instruction.Types


-- | TODO: It could fail. Change type signature
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
  | OVector (V.Vector Object)
  | ODic (M.Map T.Text Object)
  | OFunc (M.Map T.Text Object) [Word] Prog
  | OObject (Maybe Word) (M.Map T.Text Word)
  -- ^ Object instance from class Word
  | ONative ([Object] -> Prog)
  -- ^ Native object
  | ORef Word
  -- ^ Pointer reference
  | ONone

instance Show Object where
  show obj = case obj of
    OStr{}          -> "[String]"
    OBool{}         -> "[Bool]"
    ODouble{}       -> "[Double]"
    ORegex{}        -> "[Regex]"
    OShellCommand{} -> "[ShellCmd]"
    ONum{}          -> "[Int]"
    OFunc{}         -> "[Function]"
    ONative{}       -> "[Native Function]"
    ORef{}          -> "[Reference]"
    OObject{}       -> "[Object]"
    OVector{}       -> "[Vector]"
    ODic{}          -> "[Dictionary]"
    ONone           -> "[None]"

instance ToObject Object where
  toObject = id

instance FromObject Object where
  fromObject = id

instance ToObject Bool where
  toObject = OBool

instance FromObject Bool where
  fromObject (OBool bool) = bool
  fromObject _            = error "Not Implemented"

instance ToObject Int where
  toObject = ONum

instance FromObject Int where
  fromObject (ONum num) = num
  fromObject _          = error "Not Implemented"

instance ToObject Double where
  toObject = ODouble

instance FromObject Double where
  fromObject (ODouble num) = num
  fromObject _             = error "Not Implemented"
newtype ObjectError = ObjectError { info :: T.Text }
