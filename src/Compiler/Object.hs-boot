{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compiler.Object where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString           as B
import           Text.Regex.PCRE.Light

import Compiler.Types

instance AccessHierarchy StWorld Object
instance Redirection StWorld

instance ToObject a => ToObject (IO a)
instance FromObject ShellType
instance ToObject Object
instance FromObject Object
instance ToObject ()
instance FromObject ()
instance ToObject Bool
instance FromObject Bool
instance ToObject Int
instance FromObject Int
instance ToObject Double
instance FromObject Double
instance ToObject a => ToObject (V.Vector a)
instance FromObject a => FromObject (V.Vector a)
instance ToObject a => ToObject [a]
instance FromObject a => FromObject [a]
instance ToObject a => ToObject (M.Map T.Text a)
instance FromObject a => FromObject (M.Map T.Text a)
instance {-# OVERLAPPING #-} ToObject [Char]
instance {-# OVERLAPPING #-} FromObject [Char]
instance ToObject B.ByteString
instance FromObject B.ByteString
instance ToObject T.Text
instance FromObject T.Text
instance ToObject Regex
instance FromObject Regex
instance FromObject a => FromObject (Maybe a)
instance ToObject a => ToObject (Maybe a)