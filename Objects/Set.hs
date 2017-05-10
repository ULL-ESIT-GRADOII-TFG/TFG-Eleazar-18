{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module ScriptLang.Objects.Set where

import           Data.Monoid
import qualified Data.Text   as T
import           Data.Vector as V

import           ScriptLang.Objects

{- |

    TODO:
      - fromSet
      - fromVector
      - fromOther...
      - methods
        + iter
        + find
        + filter
        + union
        +
-}

instance ToObject (Vector Object) where
  toObject vec = Object
    { _internal = vec
    , _getProperty = getProperties
    , _setProperty = setProperties
    , _method = vecMethods
    , _prettyShow = showObjVector
    , _briefInfo = const "Vec"
    , objType = SetObj
    }

getProperties :: Vector Object -> Name -> StWorld (Maybe Object)
getProperties = getPropertiesFromList properties
  where
    properties = []

setProperties :: Vector Object -> Name -> Object -> StWorld (Maybe ObjectError)
setProperties = undefined

vecMethods :: Vector Object -> Name -> [Object] -> StWorld (Either ObjectError Object)
vecMethods = methodsFromList methods
  where
    methods = []

showObjVector :: Vector Object -> T.Text
showObjVector []    = "[]"
showObjVector [obj] = "[ " <> (T.pack $ show (objType obj)) <> " ]"
showObjVector vec   =
 "[ "
  <> V.foldl (\a b -> a <> ", " <> (T.pack . show $ objType b))
             (T.pack . show . objType $ V.head vec)
             vec
  <> " ]"
