{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import           Control.Monad.Except
import qualified Data.Map                     as M
import qualified Data.Text                    as T

import           Compiler.Error
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
        Nothing   -> throw $ NotFoundObject (address^.refA)
    Nothing -> throw $ NotFoundObject (address^.refA)

callObjectDirect :: Object -> [Object] -> StWorld Object
callObjectDirect obj objs = case obj of
  OFunc _ ids prog ->
    if length ids /= length objs then
      throw NumArgsMissmatch
    else
      runProgram $ do
        let argsIDs = map simple ids
        tokn <- lift . use $ currentTokenInfoA
        zipWithM_ (assign (Info mempty tokn)) argsIDs objs
        val <- prog
        mapM_ (dropVar (Info mempty tokn)) argsIDs
        return val
  ONative native   -> runProgram $ native objs
  OClassDef _name refCls methods -> do
      self <- newObject $ OObject (Just refCls) mempty
      case M.lookup "__init__" methods of
        Just method -> do
          _ <- callObjectDirect method (ORef self:objs)
          return (ORef self)
        Nothing -> if null objs then
                     return (ORef self)
                   else
                     throw NumArgsMissmatch
  _t               -> throw NotCallable

-- instanceObject :: [Object] -> FreeT Instruction StWorld Object
-- instanceObject objs = case objs of
--   (ONum idRef : args) -> do
--     defs <- use tableA
--     let classDef = IM.lookup idRef defs >>= \obj ->
--           case obj^.rawObjA of
--             OClassDef _ d -> return d
--             _             -> Nothing
--     case classDef of
--       Just clsDef -> do
--       Nothing -> lift $ throwError NotFoundObject
--   _ -> lift $ throwError $ WorldError "instanceObject: Wrong parameters"

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
    _           -> throw NotIterable

-- | Check truthfulness of an object
checkBool :: Object -> StWorld Bool
checkBool (OBool bool) = return bool
-- checkBool _obj         = error "Implement"  -- TODO: __bool__
checkBool _            = return False
