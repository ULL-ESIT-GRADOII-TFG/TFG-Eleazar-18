{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import qualified Data.Text as T

import           Compiler.Ast
import {-# SOURCE #-} Compiler.Instruction.Methods
import           Compiler.Instruction.Types
import           Compiler.Object.Types
import           Compiler.World.Methods
import           Compiler.World.Types

isNone :: Object -> Bool
isNone ONone = True
isNone _     = False

callObject :: AddressRef -> [Object] -> StWorld Object
callObject address args = do
  lookupObj <- lookupInMemory address
  case lookupObj of
    Just (obj, accessors) -> do
      mObj <- through obj accessors
      case mObj of
        Just (OFunc _ ids prog) -> runProgram (undefined ids args >> prog) -- TODO:
        Just (ONative native)   -> runProgram (native args)
        _t                      -> return ONone
    Nothing               -> return ONone

mapObj :: Object -> (Object -> StWorld Object) -> StWorld Object
mapObj obj func = case obj of
    OStr str    -> do
      -- TODO: Avoid unpack. Revisit what kind of problems there are to no exist an
      --      instance of foldable for Text
      mapM_ (\chr -> func $ OStr $ T.singleton chr) (T.unpack str)
      return ONone
    OVector vec -> mapM_ func vec >> return ONone
    ODic{}      -> undefined
    ORef word   -> follow word >>= flip mapObj func
    OObject{}   -> error "Implement"
    _           -> error "No Iterable object"

-- | Check truthfulness of an object
checkBool :: Object -> StWorld Bool
checkBool (OBool bool) = return bool
checkBool _            = return False
