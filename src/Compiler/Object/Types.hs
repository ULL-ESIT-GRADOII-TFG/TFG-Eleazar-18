{-# LANGUAGE FlexibleInstances #-}
module Compiler.Object.Types where

-- import qualified Data.Map             as M
import qualified Data.Text      as T
import qualified Data.Vector    as V

import           Compiler.Error
import           Compiler.Types

class ToObject o where
  toObject :: o -> Object

class FromObject o where
  fromObject :: Object -> StWorld o

instance ToObject Object where
  toObject = id

instance FromObject Object where
  fromObject = return

instance ToObject Bool where
  toObject = OBool

instance FromObject Bool where
  fromObject (OBool bool) = return bool
  fromObject _            = throw NotImplicitConversion

instance ToObject Int where
  toObject = ONum

instance FromObject Int where
  fromObject (ONum num) = return num
  fromObject _          = throw NotImplicitConversion

instance ToObject Double where
  toObject = ODouble

instance FromObject Double where
  fromObject (ODouble num) = return num
  fromObject _             = throw NotImplicitConversion

instance ToObject a => ToObject (V.Vector a) where
  toObject = OVector . V.map toObject

instance FromObject a => FromObject (V.Vector a) where
  fromObject (OVector vec) = mapM fromObject vec
  fromObject _             = throw NotImplicitConversion

-- TODO: Generate subvars
-- instance ToObject a => ToObject (M.Map T.Text a) where
--   toObject = OObject Nothing . M.map toObject

-- TODO: Retrieve subvars
-- instance FromObject a => FromObject (M.Map T.Text a) where
--   fromObject (OObject _ dic) = mapM fromObject dic
--   fromObject _               = throw NotImplicitConversion

instance ToObject T.Text where
  toObject = OStr

instance FromObject T.Text where
  fromObject (OStr text) = return text
  fromObject _           = throw NotImplicitConversion
