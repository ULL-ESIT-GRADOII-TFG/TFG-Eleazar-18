{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import qualified Data.Text                  as T
import           Data.Monoid

import Compiler.Ast
import Compiler.World.Types
import Compiler.Instruction.Types
import {-# SOURCE #-} Compiler.Instruction.Methods
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
  ONative {}  -> "Native"


fromAST :: Atom -> Object
fromAST atom =
  case atom of
    ANum int -> ONum int
    AStr str -> OStr str
    ADecimal double -> ODouble double
    ARegex reg -> ORegex reg
    AShellCommand cmd -> OShellCommand cmd
    ABool bool -> OBool bool

-- TODO: Set args
callObject :: Object -> [Object] -> StWorld VarAccessor
callObject (OFunc _ ids prog) objs = runProgram prog
callObject (ONative native) objs = runProgram (native objs)
callObject _ _ = return $ Raw ONone

mapObj :: Object -> (Object -> a) -> a
mapObj = undefined

checkBool :: Object -> Bool
checkBool = undefined
