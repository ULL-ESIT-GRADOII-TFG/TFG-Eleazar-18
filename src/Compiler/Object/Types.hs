{-# LANGUAGE FlexibleInstances #-}
module Compiler.Object.Types where

import           Control.Monad.Except
import qualified Data.Map                   as M
import qualified Data.Vector                as V
import qualified Data.Text                  as T

import {-# SOURCE #-} Compiler.Instruction.Types
import {-# SOURCE #-} Compiler.World.Types


class ToObject o where
  toObject :: o -> Object

class FromObject o where
  fromObject :: Object -> StWorld o

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
  fromObject = return

instance ToObject Bool where
  toObject = OBool

instance FromObject Bool where
  fromObject (OBool bool) = return bool
  fromObject _            = throwError NotImplicitConversion

instance ToObject Int where
  toObject = ONum

instance FromObject Int where
  fromObject (ONum num) = return num
  fromObject _          = throwError NotImplicitConversion

instance ToObject Double where
  toObject = ODouble

instance FromObject Double where
  fromObject (ODouble num) = return num
  fromObject _             = throwError NotImplicitConversion

instance ToObject a => ToObject (V.Vector a) where
  toObject = OVector . V.map toObject

instance FromObject a => FromObject (V.Vector a) where
  fromObject (OVector vec) = mapM fromObject vec
  fromObject _             = throwError NotImplicitConversion

instance ToObject a => ToObject (M.Map T.Text a) where
  toObject = ODic . M.map toObject

instance FromObject a => FromObject (M.Map T.Text a) where
  fromObject (ODic dic) = mapM fromObject dic
  fromObject _          = throwError NotImplicitConversion

newtype ObjectError = ObjectError { info :: T.Text }
