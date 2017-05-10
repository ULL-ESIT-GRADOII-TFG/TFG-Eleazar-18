{-# LANGUAGE OverloadedStrings #-}
module ScriptLang.Primitives where

import           Control.Monad
import           Data.Map       (Map)
import           Data.Monoid
import           Data.Set       (Set)
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Vector    (Vector)

import           ScriptLang.Env


data Primitive
  = PBool Bool
  | PInt Int
  | PDouble Double
  | PText Text
  | PSet (Set Primitive)
  | PList (Vector Primitive)
  | PDict (Map Primitive Primitive)
  | PObject MetaObj
  | PFun Env ([Primitive] -> Primitive)
  | PNone

data MetaObj = MetaObj
  { _class      :: Maybe Int
  -- ^ Which class belong to current object, if it is `Nothing` then is a singleton object
  , _additional :: Map Text (Restrictions, Primitive)
  -- ^ New methods and attributes to object
  }

data MetaClass = MetaClass
  { _id       :: Int
  -- ^ Unique identifier to class
  , _iterable :: (Primitive -> StWorld ()) -> MetaObj -> StWorld Primitive
  -- ^ Map an object and return the same object with modifications in loop
  , _boolean  :: MetaObj -> StWorld Bool
  -- ^ Test if the object could be a logical predicate
  , _objects  :: Map Text (Restrictions, Primitive)
  -- ^ Definition of class: attributes and methods shared by object instances
  , _info     :: MetaObj -> StWorld Text
  -- ^ A tiny information about object
  , _builder  :: [Primitive] -> StWorld (IORef Primitive)
  -- ^ Build a primitive from parameters
  }


data Restrictions = RW | RO | None deriving (Eq, Show)

data Identifier = Plain Text | Operator Text deriving (Ord, Eq, Show)




infoPrim :: Primitive -> T.Text
infoPrim prim = case prim of
  PNone     -> ""
  PBool b   -> "Bool:" <> T.pack (show b)
  PInt i    -> "Int:" <> T.pack (show i)
  PDouble i -> "Int:" <> T.pack (show i)
  PText t   -> "Text:" <> (if T.length t > 8 then T.take 5 t <> "..." else t)
  PFun _ _  -> "Fun"
  PList _   -> "List"
  PDict _   -> "Dict"
  PObject _ -> "Object" -- TODO: Take from meta data
  PClass _  -> "Class"


isPNone :: Primitive -> Bool
isPNone PNone = True
isPNone _     = False

checkBoolean :: Primitive -> Bool
checkBoolean _boolean = undefined -- TODO

prettyShowPrim :: Primitive -> T.Text
prettyShowPrim = undefined
