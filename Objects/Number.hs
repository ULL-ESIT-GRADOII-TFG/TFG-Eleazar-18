module ScriptLang.Objects.Number where

import qualified Data.Text as T

import ScriptLang.Types
import ScriptLang.Objects


instance ToObject Int where
  toObject int = Object
    { objType = Num
    , _internal = int
    , _getProperty = defaultGetProperties
    , _setProperty = defaultSetProperties
    , _method = defaultMethod
    , _prettyShow = T.pack . show
    , _briefInfo = T.pack . (\num -> "Num(" ++ num ++ ")") . show
    }
