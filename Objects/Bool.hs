module ScriptLang.Objects.Bool where

import qualified Data.Text          as T

import           ScriptLang.Objects


instance ToObject Bool where
  toObject bool = Object
    { objType = Boolean
    , _internal = bool
    , _getProperty = defaultGetProperties
    , _setProperty = defaultSetProperties
    , _method = defaultMethod
    , _prettyShow = T.pack . show
    , _briefInfo = T.pack . show
    }
