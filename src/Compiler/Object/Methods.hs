{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import           Control.Monad.Except
import qualified Data.Text                    as T

import {-# SOURCE #-} Compiler.Instruction.Methods
import           Compiler.Types
import           Compiler.World.Methods


-- | From memory address, check if object callable and call it with given arguments
callObject :: AddressRef -> [Object] -> StWorld Object
callObject address args = do
  lookupObj <- lookupInMemory address
  case lookupObj of
    Just (obj, accessors) -> do
      mObj <- through obj accessors
      let args' = if null accessors then args else obj:args
      case mObj of
        Just obj' -> callObjectDirect obj' args'
        Nothing   -> throwError NotFoundObject
    Nothing -> throwError NotFoundObject

callObjectDirect :: Object -> [Object] -> StWorld Object
callObjectDirect obj objs = case obj of
  OFunc _ ids prog ->
    if length ids /= length objs then
      throwError NumArgsMissmatch
    else
      runProgram $ do
        let argsIDs = map simple ids
        --- zipWithM_ (=:) argsIDs objs
        val <- prog
        -- mapM_ dropVar argsIDs
        return val
  ONative native   -> runProgram $ native objs
  OClassDef _name methods -> undefined
  _t               -> throwError NotCallable

-- | Iterate over a object if it is iterable
mapObj :: Object -> (Object -> StWorld Object) -> StWorld Object
mapObj obj func = case obj of
    OStr str    -> do
      -- TODO: Avoid unpack. Revisit what kind of problems there are to no exist an
      --      instance of foldable for Text
      mapM_ (func . OStr . T.singleton) (T.unpack str)
      return ONone
    OVector vec -> mapM_ func vec >> return ONone
    ORef word   -> follow word >>= flip mapObj func
    OObject{}   -> error "Implement" -- TODO: __iter__ or __map__
    _           -> throwError NotIterable

-- | Check truthfulness of an object
checkBool :: Object -> StWorld Bool
checkBool (OBool bool) = return bool
checkBool _obj         = error "Implement"  -- TODO: __bool__
checkBool _            = return False
