{-# LANGUAGE FlexibleInstances #-}
module Compiler.Object.Types where

-- import qualified Data.Map             as M
import qualified Data.ByteString       as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Vector           as V
import           Text.Regex.PCRE.Light

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

instance ToObject () where
  toObject () = ONone

instance FromObject () where
  fromObject ONone = return ()
  fromObject _     = throw NotImplicitConversion

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

instance ToObject a => ToObject [a] where
  toObject = OVector . V.map toObject . V.fromList

instance FromObject a => FromObject [a] where
  fromObject (OVector vec) = V.toList <$> mapM fromObject vec
  fromObject _             = throw NotImplicitConversion

-- TODO: Generate subvars
-- instance ToObject a => ToObject (M.Map T.Text a) where
--   toObject = OObject Nothing . M.map toObject

-- TODO: Retrieve subvars
-- instance FromObject a => FromObject (M.Map T.Text a) where
--   fromObject (OObject _ dic) = mapM fromObject dic
--   fromObject _               = throw NotImplicitConversion

-- There aren't a char type equivalent right now. Remind: Two paths [Char] String
-- instance ToObject Char where
--   toObject = OStr . T.singleton

instance {-# OVERLAPPING #-} ToObject [Char] where
  toObject = OStr . T.pack

instance {-# OVERLAPPING #-} FromObject [Char] where
  fromObject (OStr text) = return (T.unpack text)
  fromObject _           = throw NotImplicitConversion

instance ToObject B.ByteString where
  toObject = OStr . TE.decodeUtf8

instance FromObject B.ByteString where
  fromObject (OStr text) = return $ TE.encodeUtf8 text
  fromObject _           = throw NotImplicitConversion

instance ToObject T.Text where
  toObject = OStr

instance FromObject T.Text where
  fromObject (OStr text) = return text
  fromObject _           = throw NotImplicitConversion

instance ToObject Regex where
  toObject = ORegex

instance FromObject a => FromObject (Maybe a) where
  fromObject ONone = return Nothing
  fromObject obj   = Just <$> fromObject obj

instance ToObject a => ToObject (Maybe a) where
  toObject = maybe ONone toObject

instance FromObject Regex where
  fromObject (ORegex regex) = return regex
  fromObject _              = throw NotImplicitConversion
