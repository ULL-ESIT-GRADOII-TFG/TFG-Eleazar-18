{-# LANGUAGE OverloadedStrings #-}
module ScriptLang.Objects.LitString where

import           Data.Text     (Text)

import           ScriptLang.Objects


instance ToObject Text where
  toObject text = Object
    { _internal = text
    , _getProperty = defaultGetProperties
    , _setProperty = defaultSetProperties
    , _method = defaultMethod
    , _prettyShow = id
    , _briefInfo = id
    , objType = LitStr
    }
