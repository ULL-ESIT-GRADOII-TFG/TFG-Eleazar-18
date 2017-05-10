{-# LANGUAGE OverloadedStrings #-}
module ScriptLang.Objects.Void where

import           ScriptLang.Objects

-- | Nothing. A simple nil, void to represent nothing
newVoid :: StWorld (Either ObjectError Object)
newVoid = return $ Right (toObject ())

instance ToObject () where
  toObject _ = Object
    { _internal = ()
    , _getProperty = defaultGetProperties
    , _setProperty = defaultSetProperties
    , _method = defaultMethod
    , _prettyShow = const "Void"
    , _briefInfo = const ""
    , objType = Void
    }
