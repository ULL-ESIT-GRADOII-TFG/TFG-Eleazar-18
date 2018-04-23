{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import           Compiler.Ast
import {-# SOURCE #-} Compiler.Instruction.Methods
import           Compiler.Instruction.Types
import           Compiler.Object.Types
import           Compiler.World.Methods
import           Compiler.World.Types
import Debug.Trace

isNone :: Object -> Bool
isNone ONone = True
isNone _     = False

-- | Transform literal data from AST to an object
fromAST :: Atom -> Object
fromAST atom =
  case atom of
    ANum int          -> ONum int
    AStr str          -> OStr str
    ADecimal double   -> ODouble double
    ARegex reg        -> ORegex reg
    AShellCommand cmd -> OShellCommand cmd
    ABool bool        -> OBool bool

callObject :: AddressRef -> [Object] -> StWorld Object
callObject address args = do
  lookupObj <- lookupInMemory address
  case lookupObj of
    Just (obj, accessors) -> do
      mObj <- through obj accessors
      case mObj of
        Just (OFunc _ ids prog) -> runProgram (undefined ids args >> prog) -- TODO:
        Just (ONative native)   -> traceShow "Calling" runProgram (native args)
        t                       -> traceShow (show t) return ONone
    Nothing               -> return ONone

mapObj :: Object -> (Object -> StWorld Object) -> StWorld Object
mapObj = undefined -- TODO: Implement iterable objects __map__

-- | Check truthfulness of an object
checkBool :: Object -> StWorld Bool
checkBool (OBool bool) = return bool
checkBool _            = return False

