{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import           Control.Monad.Except
import qualified Data.Map                     as M
import qualified Data.Text                    as T

import           Compiler.Error
import {-# SOURCE #-} Compiler.Instruction.Methods
import           Compiler.Prelude.Types
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
        Just obj' ->
          if null accessors then
            callObjectDirect obj' args'
          else
            catchArgsMethodsError $ callObjectDirect obj' args'
        Nothing   -> throw $ NotPropertyFound [] (T.intercalate "." (address^.dynPathA)) []
    Nothing -> throw $ NotFoundObject (address^.refA)

callObjectDirect :: Object -> [Object] -> StWorld Object
callObjectDirect obj objs = case obj of
  OFunc _ ids prog ->
    if length ids /= length objs then
      throw $ NumArgsMissmatch (length ids) (length objs)
    else
      runProgram $ do
        let argsIDs = map simple ids
        tokn <- lift . use $ currentTokenInfoA
        zipWithM_ (assign (Info mempty tokn)) argsIDs objs
        val <- prog
        mapM_ (dropVar (Info mempty tokn)) argsIDs
        return val

  ONative native -> runProgram $ native objs

  OClassDef _name refCls methods -> do
      self <- newObject $ OObject (Just refCls) mempty
      case M.lookup "__init__" methods of
        Just method -> do
          _ <- catchArgsMethodsError $ callObjectDirect method (ORef self:objs)
          return (ORef self)
        Nothing ->
          if null objs then
            return (ORef self)
          else
            throw $ NumArgsMissmatch 0 (length objs)

  t -> throw $ NotCallable (typeName t)

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
    OObject (Just classRef) _attrs -> do
      clsObj <- findObject (simple classRef)
      case clsObj of
        OClassDef _name _ref methods ->
          case M.lookup "__map__" methods of
            Just func' -> callObjectDirect func' [obj, ONative $ normalize func]
            Nothing    -> return ONone
        o  -> throw $ NotIterable (typeName o)
    o -> throw $ NotIterable (typeName o)

-- | Check truthfulness of an object
checkBool :: Object -> StWorld Bool
checkBool obj = case obj of
  OBool bool -> return bool
  OObject (Just classRef) _attrs -> do
    clsObj <- findObject (simple classRef)
    case clsObj of
      OClassDef _name _ref methods ->
        case M.lookup "__bool__" methods of
          Just func' -> callObjectDirect func' [obj] >>= checkBool
          Nothing    -> throw $ NotBoolean "Object"
      o  -> throw $ NotIterable (typeName o)
  _ -> throw $ NotBoolean (typeName obj)


typeName :: Object -> String
typeName obj = case obj of
  OClassDef{}     -> "ClassDef"
  ONative{}       -> "Native"
  OFunc{}         -> "Function"
  OStr{}          -> "Str"
  ORegex{}        -> "Regex"
  OShellCommand{} -> "ShellCommand"
  ODouble{}       -> "Double"
  OBool{}         -> "Bool"
  ONum{}          -> "Num"
  OVector{}       -> "Vector"
  ORef{}          -> "Ref"
  ONone           -> "None"
  OObject{}       -> "Object"
