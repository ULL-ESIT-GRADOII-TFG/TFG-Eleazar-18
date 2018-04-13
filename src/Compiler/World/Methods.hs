{-# LANGUAGE LambdaCase #-}
module Compiler.World.Methods where

import qualified Data.Text as T
import qualified Data.IntMap                as IM
import qualified Data.Map                as M
import Lens.Micro.Platform

import Compiler.World.Types
import Compiler.Instruction.Types
import Compiler.Object.Types


addObject :: AddressRef -> Object -> StWorld ()
addObject (AddressRef word dyns) obj = do
  table' <- use table
  case IM.lookup (fromIntegral word) table' of
    Just var@Var{} ->
      if null dyns then
        table %= IM.insert (fromIntegral word) (Var 0 obj)
      else do
        mRef <- getLastRef (var ^. rawVar) dyns
        case mRef of
          Just ref' ->
            table %= IM.insert (fromIntegral ref') (Var 0 obj)
          Nothing -> return ()
    Nothing -> table %= IM.insert (fromIntegral word) (Var 0 obj)

  where

getLastRef :: Object -> [T.Text] -> StWorld (Maybe Word)
getLastRef _ [] = return Nothing
getLastRef (OObject mClassId dic) (x:xs) =
  -- Search inner object dictionary
  case M.lookup x dic of
    Just ref' ->
      if null xs then
        return $ Just ref'
      else do
        obj <- follow ref'
        getLastRef obj xs
    Nothing -> do
      -- Search into class definition of object
      classes <- use typeDefinitions
      let
        mRef = (do
          classId <- mClassId
          classDef <- IM.lookup (fromIntegral classId) classes
          M.lookup x (classDef^.values))
      case mRef of
        Just ref' ->
          if null xs then
            return $ Just ref'
          else do
            obj <- follow ref'
            getLastRef obj xs
        Nothing ->
          -- TODO: Generate a new accessor. Needs actual reference to object
          return Nothing
getLastRef (ORef word) xs =
  if null xs then
    return $ Just word
  else do
    obj <- follow word
    getLastRef obj xs -- TODO: Problem
getLastRef _ _ = return Nothing -- TODO: Specific methods of primitive objects

findVar :: AddressRef -> StWorld Object
findVar (AddressRef word dyns) = do
  table' <- use table
  case IM.lookup (fromIntegral word) table' of
    Just var@Var{} -> do
      mRef <- getLastRef (var^.rawVar) dyns
      case mRef of
        Just ref' -> follow ref'
        Nothing -> return ONone
    Nothing -> return ONone

-- | TODO:
dropVarWorld :: AddressRef -> StWorld ()
dropVarWorld = undefined

getObject :: Object -> StWorld Object
getObject (ORef word) = findVar (simple word)
getObject obj = return obj

-- TODO: Recursive links
follow :: Word -> StWorld Object
follow word = do
  obj <- findVar (simple word)
  case obj of
    ORef word' -> follow word'
    other -> return other
