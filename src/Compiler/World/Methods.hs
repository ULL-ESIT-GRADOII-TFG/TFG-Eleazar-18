module Compiler.World.Methods where

import           Control.Monad
import qualified Data.IntMap              as IM
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Data.Maybe
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Prelude.Methods
import           Compiler.Types


-- | Add and object to memory. Address specify route to put Object
-- > obj = {}
-- > obj.foo.bar = 5
-- > obj
-- { foo: { bar: 5 } }
-- > obj = "foo"
-- > obj
-- "foo"
addObject :: AddressRef -> Object -> StWorld ()
addObject (AddressRef word dyns) obj = do
  word <- buildFollowingPath word dyns
  setVar word (Var 1 obj)

-- | Build objects following a given path `path` from initial address `addr`.
-- It returns last object following path.
-- > obj = {}
-- > obj.foo.bar
-- > obj
-- { foo: { bar: ONone } }
buildFollowingPath :: Word -> [T.Text] -> StWorld Word
buildFollowingPath addr path obj =
  flip . flip foldM $ addr path $ \addrCurrent acc -> do
    obj <- getObject addrCurrent
    mAddr <- on' obj acc
    case mAddr of
      Just addr -> addr
      Nothing ->
        addObjectToObject addrCurrent acc ONone

-- | Like `on` but doesn't perform a internal search or class search. Its mainly used to
-- modify objects not to access them
on' :: Object -> T.Text -> StWorld (Maybe Word)
on' obj acc = case obj of
  OObject _ dicObj -> return $ IM.lookup acc dicObj
                      -- ^ Local search
  ORef addr -> follow addr >>= (`on` acc)
  _         -> return Nothing

addObjectToObject :: Word -> T.Text -> Object -> StWorld Word
addObjectToObject word obj = do
  var <- getVar word
  addr <- liftScope $ getNewId
  case var^.rawObj of
    ONone ->
      setVar addr (var.~rawObj (Object Nothing (M.singleton acc addr)))
    OObject parent attrs ->
      setVar addr (var.~rawObj (Object parent (M.insert acc addr attrs)))
    _ -> throwError NotExtensibleObject
  return addr

-- | Access through an object
on :: Object -> T.Text -> StWorld (Maybe Object)
on obj acc = case obj of
  OObject mClassId dicObj ->
    attemps
        [ maybe (return Nothing) getObject $ IM.lookup acc dicObj
        -- ^ Local search
        , do
          clsDef <- use $ memory.typeDefinitions
          maybe (return Nothing) return $
            mClassId >>= (`IM.lookup` clsDef) >>= M.lookup acc
        -- ^ Class search
        , getMethods obj acc
        -- ^ Internal search
        ]
  ORef addr -> follow addr >>= (`on` acc)
  _         -> return $ ONative <$> getMethods obj acc

-- | Return the first `Just` get from list else try next
attemps :: Monad m => [m (Maybe a)] -> m (Maybe a)
attemps [] = return Nothing
attemps (x:xs) = fromMaybe (attemps xs) =<< x

-- | Access through a path accessors
through :: Object -> [T.Text] -> StWorld (Maybe Object)
through obj = foldM (\obj' acc ->
    case obj' of
      Just obj'' -> obj'' `on` acc
      Nothing    -> return Nothing
  ) (Just obj)

setVar :: Word -> Var -> StWorld ()
setVar addr var = table %= IM.insert (fromIntegral word) var

getVar :: Word -> StWorld (Maybe Var)
getVar addr = IM.lookup (fromIntegral word) <$> use table

-- | Find object. Return `ONone` in case of can't found it
findObject :: AddressRef -> StWorld Object
findObject addr = do
  (obj, dyns) <- fromMaybe (ONone, []) <$> lookupInMemory addr
  fromMaybe ONone <$> through obj dyns

lookupInMemory :: AddressRef -> StWorld (Maybe (Object, [T.Text]))
lookupInMemory (AddressRef word accessors) =
  getVar word <&> \var -> (var ^. rawObj, accessors)

-- TODO: Revisar el tema ya que se pasa un AddressRef los dyns
--       se deberian de ver si son innecesarios. En general, estudiar
--       cual debe ser el comportamiento del GC cuando se usan Vec y Object
-- | Drops a variable when its reference counter reaches 0
dropVarWorld :: AddressRef -> StWorld ()
dropVarWorld addrRef = do
  val <- getVar (addrRef^.ref)
  case val of
    Just var -> do
      let var' = var%~refCount (+ -1)
      if var'^.refCount == 0 then
        table %= IM.remove word
      else
        setVar (addrRef^.ref) var'
    Nothing -> throwError DropVariableAlreadyDropped

-- | Get a value object
getObject :: Object -> StWorld Object
getObject (ORef word) = findObject (simple word)
getObject obj         = return obj

-- | Follow reference pointer until and not reference object.
-- It throws a exception when reaches the limit 50
follow :: Word -> StWorld Object
follow word = follow' word 50
  where
    follow' word times
      | times > 0 = throwError ExcededRecursiveLimit
      | otherwise = do
        obj <- findObject (simple word)
        case obj of
          ORef word' -> follow' word' (times - 1)
          other      -> return other
