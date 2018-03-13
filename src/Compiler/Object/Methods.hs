{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import qualified Data.Text                  as T
import           Data.Monoid

import Compiler.Ast
import Compiler.Object.Types

isNone :: Object -> Bool
isNone ONone = True
isNone _     = False

infoPrim :: Object -> T.Text
infoPrim prim = case prim of
  ONone     -> ""
  OBool b   -> "Bool:" <> T.pack (show b)
  ONum i    -> "Int:" <> T.pack (show i)
  ODouble i -> "Int:" <> T.pack (show i)
  OStr t    -> "Text:" <> (if T.length t > 8 then T.take 5 t <> "..." else t)
  OFunc {}  -> "Fun"


fromAST :: Atom -> Object
fromAST atom =
  case atom of
    ANum int -> ONum int
    AStr str -> OStr str

callObject :: Object -> [Object] -> Object
callObject _ _ = ONone

mapObj :: Object -> (Object -> a) -> a
mapObj = undefined

checkBool :: Object -> Bool
checkBool = undefined
