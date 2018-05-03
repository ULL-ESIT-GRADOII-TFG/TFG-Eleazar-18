{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import           Control.Monad.Except
import qualified Data.Text                    as T

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
      let args' = if null accessors then args else obj:args
      case mObj of
        Just (OFunc _ ids prog) -> runProgram (sequence_ (zipWith (=:) (map simple ids) args') >> prog)
        Just (ONative native)   -> runProgram $ native args'
        _t                      -> throwError NotCallable
    Nothing               -> throwError NotFoundObject

mapObj :: Object -> (Object -> StWorld Object) -> StWorld Object
mapObj obj func = case obj of
    OStr str    -> do
      -- TODO: Avoid unpack. Revisit what kind of problems there are to no exist an
      --      instance of foldable for Text
      mapM_ (\chr -> func $ OStr $ T.singleton chr) (T.unpack str)
      return ONone
    OVector vec -> mapM_ func vec >> return ONone
    ODic{}      -> error "implement"
    ORef word   -> follow word >>= flip mapObj func
    OObject{}   -> error "Implement"
    _           -> throwError NotIterable

-- | Check truthfulness of an object
checkBool :: Object -> StWorld Bool
checkBool (OBool bool) = return bool
checkBool _            = return False
