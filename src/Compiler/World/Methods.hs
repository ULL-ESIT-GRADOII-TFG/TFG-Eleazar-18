{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.World.Methods where

import           Control.Monad
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import           Compiler.Instruction.Types
import           Compiler.Object.Types
import           Compiler.World.Types


-- | Add and object to memory. Address specify route to put Object
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

-- | Access through an object
on :: Object -> T.Text -> StWorld (Maybe Object)
on obj acc = return $ ONative <$> getMethods obj acc

-- | Access through a path accessors
through :: Object -> [T.Text] -> StWorld (Maybe Object)
through obj accessor = foldM (\obj' acc ->
    case obj' of
      Just obj'' -> obj'' `on` acc
      Nothing    -> return Nothing
  ) (Just obj) accessor

lookupInMemory :: AddressRef -> StWorld (Maybe (Object, [T.Text]))
lookupInMemory (AddressRef word accessors) = do
  table' <- use table
  case IM.lookup (fromIntegral word) table' of
    Just var@Var{} -> return $ Just (var ^. rawVar, accessors)
    Nothing        -> return Nothing

getMethods :: Object -> T.Text -> Maybe ([Object] -> Prog)
getMethods obj name = case obj of
  OStr str               ->
    case name of
      "++" -> Just $ \objs -> return ONone
      _ -> Nothing
  OBool val              -> Nothing
  ODouble val            -> Nothing
  ONum val               -> Nothing
  ORegex str             -> Nothing
  OShellCommand str      -> Nothing
  OFunc bind args prog   -> Nothing
  OObject classId dicObj -> Nothing
  ORef rfs               -> Nothing
  ONone                  -> Nothing

-- | Helper to object reference by `AddressRef`
--   TODO: Try to remove, too verbose
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
        mRef = do
          classId <- mClassId
          classDef <- IM.lookup (fromIntegral classId) classes
          M.lookup x (classDef^.values)

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
getLastRef obj values = return Nothing -- TODO: Specific methods of primitive objects

-- | Find object
-- TODO: getMethods in case of fail
findObject :: AddressRef -> StWorld Object
findObject (AddressRef word dyns) = do
  table' <- use table
  case IM.lookup (fromIntegral word) table' of
    Just var@Var{} -> do
      mRef <- getLastRef (var^.rawVar) dyns
      case mRef of
        Just ref' -> follow ref'
        Nothing   -> return ONone
    Nothing -> return ONone

-- | TODO:
dropVarWorld :: AddressRef -> StWorld ()
dropVarWorld = undefined

-- | Get a value object
getObject :: Object -> StWorld Object
getObject (ORef word) = findObject (simple word)
getObject obj         = return obj

-- | Follow reference pointer until and not reference object
-- TODO: Throw error on Recursive links
follow :: Word -> StWorld Object
follow word = do
  obj <- findObject (simple word)
  case obj of
    ORef word' -> follow word'
    other      -> return other
