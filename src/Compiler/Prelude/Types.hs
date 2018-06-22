{-# LANGUAGE FlexibleInstances #-}
module Compiler.Prelude.Types where

import           Control.Monad.Except
import           Control.Monad.Trans.Free

import           Compiler.Error
import           Compiler.Object.Types
import           Compiler.Types


data Assoc = LeftAssoc | RightAssoc deriving (Show, Eq)

-- | Tranform haskell curryfied functions into single list argument function
class Normalize a where
  {-# MINIMAL normalize' #-}
  normalize :: a -> [Object] -> FreeT Instruction StWorld Object
  normalize a objs = normalize' a 0 (length objs) objs
  -- | Same to normalize but take account of numbers of args expected and given
  normalize' :: a -> Int -> Int -> [Object] -> FreeT Instruction StWorld Object

instance Normalize (FreeT Instruction StWorld Object)  where
  normalize' f expected given ls =
    case ls of
      [] -> f
      _  -> throw $ NumArgsMissmatch expected given

instance Normalize Object where
  normalize' f expected given ls =
    case ls of
      [] -> return f
      _  -> throw $ NumArgsMissmatch expected given

instance ToObject a => Normalize (IO a) where
  normalize' f expected given ls =
    case ls of
      [] -> toObject <$> liftIO f
      _  -> throw $ NumArgsMissmatch expected given

instance ToObject a => Normalize (StWorld a) where
  normalize' f expected given ls =
    case ls of
      [] -> toObject <$> lift f
      _  -> throw $ NumArgsMissmatch expected given

instance (Normalize r, FromObject a) => Normalize (a -> r) where
  normalize' fun expected given []     =
    normalize' (fun undefined) (expected + 1) given [] -- It doesn't should be a problem
  normalize' fun expected given (a:xs) = do
    obj <- lift $ fromObject a
    normalize' (fun obj) (expected+1) given xs
