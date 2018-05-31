module Compiler.World.Methods where

import           Control.Monad
import qualified Data.IntMap              as IM
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Prelude.Methods
import           Compiler.Types


-- | Add and object to memory. Address specify route to put Object
addObject :: AddressRef -> Object -> StWorld ()
addObject (AddressRef word dyns) obj = do
  table' <- use table
  case IM.lookup (fromIntegral word) table' of
    Just var@Var{} ->
      if null dyns then
        table %= IM.insert (fromIntegral word) (Var 0 obj)
      else do
        mRef <- getLastRef (var ^. rawObj) dyns
        case mRef of
          Just ref' ->
            table %= IM.insert (fromIntegral ref') (Var 0 obj)
          Nothing -> return ()
    Nothing -> table %= IM.insert (fromIntegral word) (Var 0 obj)

-- | Access through an object
on :: Object -> T.Text -> StWorld (Maybe Object)
on obj acc = case obj of
  OObject mClassId _dicObj ->
    case mClassId of
      Just classId -> do
        classDefs <- use $ scope.typeDefinitions
        case IM.lookup (fromIntegral classId) classDefs of
          Just classDef ->
            return $ M.lookup acc (classDef ^. attributesClass)
          Nothing -> return Nothing
      Nothing -> return Nothing
  ORef _rfs               -> return Nothing
  _                  -> return $ ONative <$>  getMethods obj acc

-- | Access through a path accessors
through :: Object -> [T.Text] -> StWorld (Maybe Object)
through obj = foldM (\obj' acc ->
    case obj' of
      Just obj'' -> obj'' `on` acc
      Nothing    -> return Nothing
  ) (Just obj)

lookupInMemory :: AddressRef -> StWorld (Maybe (Object, [T.Text]))
lookupInMemory (AddressRef word accessors) = do
  table' <- use table
  case IM.lookup (fromIntegral word) table' of
    Just var@Var{} -> return $ Just (var ^. rawObj, accessors)
    Nothing        -> return Nothing

-- | Helper to object reference by `AddressRef`
--   TODO: Try to remove, too verboseqe
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
      classes <- use $ scope . typeDefinitions
      let
        mRef = do
          classId <- mClassId
          classDef <- IM.lookup (fromIntegral classId) classes
          M.lookup x (classDef ^. attributesClass)

      case mRef of
        Just ref' ->
          return Nothing
          -- if null xs then
          --   return $ Just ref'
          -- else do
          --   obj <- follow ref'
          --   getLastRef obj xs
        Nothing ->
          -- TODO: Generate a new accessor. Needs actual reference to object
          return Nothing
getLastRef (ORef word) xs =
  if null xs then
    return $ Just word
  else do
    obj <- follow word
    getLastRef obj xs -- TODO: Problem
getLastRef _obj _values = return Nothing -- TODO: Specific methods of primitive objects

-- | Find object
findObject :: AddressRef -> StWorld Object
findObject (AddressRef word dyns) = do
  table' <- use table
  case IM.lookup (fromIntegral word) table' of
    Just var@Var{} -> do
      mRef <- through (var ^. rawObj) dyns
      case mRef of
        Just ref' -> return ref'
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
