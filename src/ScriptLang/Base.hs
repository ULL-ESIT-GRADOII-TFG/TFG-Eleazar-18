{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ScriptLang.Base where

import           Control.Monad.IO.Class
import qualified Data.HashMap.Lazy      as M
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Lens.Simple

import           ScriptLang.Primitives


class Normalize a where
  normalize :: a -> [Primitive] -> StWorld (Either ObjectError Primitive)

instance Normalize (StWorld (Either ObjectError Primitive)) where
  normalize action [] = action
  normalize _ _       = return $ Left (ObjectError "Wrong number of params")

instance Normalize a => Normalize (Primitive -> a) where
  normalize _   []     = return $ Left (ObjectError "Wait more params")
  normalize fun (a:xs) = normalize (fun a) xs

-- | Basic constructors primitives
basicsFunctions :: T.Text -> [Primitive] -> StWorld (Either ObjectError Primitive)
basicsFunctions name params =
  case M.lookup name (M.fromList functions) of
    Just method' -> method' params
    Nothing      -> return (Left (ObjectError "Not basic functions found"))

functions :: [(T.Text, [Primitive] -> StWorld (Either ObjectError Primitive))]
functions =
  [ ("cd", normalize cdFun)
  , ("ls", normalize lsFun)
  , ("show", normalize showFun)
  ]

cdFun :: Primitive -> StWorld (Either ObjectError Primitive)
cdFun prim = do
  implicit .= prim
  return $ Right PNone

lsFun :: [Primitive] -> StWorld (Either ObjectError Primitive)
lsFun []  = do
  prim <- use implicit
  return $ Right (attributes prim <> getMethods prim)
lsFun [prim] = return $ Right (attributes prim <> getMethods prim)

showFun :: Primitive -> StWorld (Either ObjectError Primitive)
showFun obj = (liftIO . T.putStrLn $ prettyShow obj) >> newVoid

-- | This function set order to search a property or method in object
-- First check, if it is a property get or set. Then search in methods.
searchIntoObject :: [Primitive] -> Primitive -> T.Text -> StWorld (Either ObjectError Primitive)
searchIntoObject [] self name = do
  mProp <- getProperty self name
  case mProp of
    Just obj -> return $ Right obj
    Nothing  -> method self name []
searchIntoObject [obj] self name = do
  mProp <- setProperty self name obj
  case mProp of
    Nothing -> newVoid
    Just _  -> method self name [obj]
searchIntoObject objs self name = method self name objs
