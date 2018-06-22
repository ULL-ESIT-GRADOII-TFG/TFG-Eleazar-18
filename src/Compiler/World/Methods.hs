module Compiler.World.Methods where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.IntMap                    as IM
import qualified Data.Map                       as M
import           Data.Maybe
import qualified Data.Text                      as T
import           Lens.Micro.Platform

import           Compiler.Error
import           Compiler.Identifier
import qualified Compiler.Prelude.OBool         as OBool
import qualified Compiler.Prelude.ODouble       as ODouble
import qualified Compiler.Prelude.ONum          as ONum
import qualified Compiler.Prelude.ORegex        as ORegex
import qualified Compiler.Prelude.OShellCommand as OShellCommand
import qualified Compiler.Prelude.OStr          as OStr
import qualified Compiler.Prelude.OVector       as OVector
import           Compiler.Scope.Utils
import           Compiler.Types


-- | Set a var into memory
setVar :: Word -> Var -> StWorld ()
setVar addr var = innerStateA.tableA %= IM.insert (fromIntegral addr) var

-- | Get a var from memory
getVar :: Word -> StWorld (Maybe Var)
getVar addr = IM.lookup (fromIntegral addr) <$> use (innerStateA.tableA)

-- | Create a new object in memory.
newObject :: Object -> StWorld Word
newObject obj = do
  addr <- liftIO getNewID
  addObject (simple addr) obj
  return addr

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
  word' <- buildFollowingPath word dyns
  setVar word' (Var 1 obj)

-- | Build objects following a given path `path` from initial address `addr`.
-- It returns last object following path.
-- > obj = {}
-- > obj.foo.bar # No currently possible to do in this way
-- > obj
-- { foo: { bar: ONone } }
buildFollowingPath :: Word -> [T.Text] -> StWorld Word
buildFollowingPath addr path =
  (flip . flip foldM) addr path $ \addrCurrent acc -> do
    lastRef <- follow' addrCurrent
    mAddr <- on' (ORef lastRef) acc
    case mAddr of
      Just addr' -> return addr'
      Nothing ->
        addObjectToObject lastRef acc ONone

-- | Like `on` but doesn't perform a internal search or class search. Its mainly used to
-- modify objects not to access them
on' :: Object -> T.Text -> StWorld (Maybe Word)
on' obj acc = case obj of
  OObject _ dicObj -> return $ M.lookup acc dicObj
                      -- Local search
  ORef addr        -> follow addr >>= (`on'` acc)
  _                -> return Nothing

-- | Get a address and try to add a new accesor to it
addObjectToObject :: Word -> T.Text -> Object -> StWorld Word
addObjectToObject word acc obj = do
  var <- do
    lastAddr <- follow' word
    var <- getVar lastAddr
    case var of
      Just var' -> return var'
      Nothing   -> throw NotExtensibleObject
  addr <- liftIO getNewID

  case var^.rawObjA of
    ONone -> do
      setVar word (var & rawObjA.~ OObject Nothing (M.singleton acc addr))
      setVar addr (Var 1 obj)
    OObject parent attrs -> do
      setVar word (var & rawObjA.~ OObject parent (M.insert acc addr attrs))
      setVar addr (Var 1 obj)
    _ -> throw NotExtensibleObject
  return addr

-- | Access through an object
on :: Object -> T.Text -> StWorld (Maybe Object)
on obj acc = case obj of
  OObject mClassId dicObj ->
    attemps
        -- Local search
        [ maybe (return Nothing) (fmap Just . follow) $ M.lookup acc dicObj
        -- Class search
        , do
          memory <- use (innerStateA.tableA)
          return $ mClassId
            >>= (`IM.lookup` memory) . fromIntegral
            >>= (\obj' -> case obj'^.rawObjA of
                    OClassDef{} -> return $ attributesClass (obj'^.rawObjA)
                    _           -> Nothing)
            >>= M.lookup acc
        -- Internal search
        , return $ ONative <$> getMethods obj acc
        ]
  ORef addr -> follow addr >>= (`on` acc)
  _         -> return $ ONative <$> getMethods obj acc

catchArgsMethodsError :: StWorld a -> StWorld a
catchArgsMethodsError stw = catchError stw onNumArgsMissmatch
  where
    onNumArgsMissmatch :: ErrorInfo WorldError -> StWorld a
    onNumArgsMissmatch errTy = case errTy of
      ErrorInfo info (NumArgsMissmatch expected given) ->
        throwError $ ErrorInfo info (NumArgsMissmatch (expected - 1) (given - 1))
      err'                            -> throwError err'

-- | Return the first `Just` get from list else try next
attemps :: Monad m => [m (Maybe a)] -> m (Maybe a)
attemps [] = return Nothing
attemps (x:xs) = do
  val <- x
  case val of
    Just a  -> return $ Just a
    Nothing -> attemps xs

-- | Access through a path accessors
through :: Object -> [T.Text] -> StWorld (Maybe Object)
through obj = foldM (\obj' acc ->
    case obj' of
      Just obj'' -> obj'' `on` acc
      Nothing    -> return Nothing
  ) (Just obj)

-- TODO Reorganize to add internal docs
getMethods :: Object -> T.Text -> Maybe ([Object] -> Prog)
getMethods obj name = case obj of
  OStr{}          -> OStr.methods name
  OBool{}         -> OBool.methods name
  ODouble{}       -> ODouble.methods name
  ONum{}          -> ONum.methods name
  OVector{}       -> OVector.methods name
  ORegex{}        -> ORegex.methods name
  OShellCommand{} -> OShellCommand.methods name
  OFunc{}         -> Nothing
  ONative{}       -> Nothing
  OObject{}       -> Nothing
  OClassDef{}     -> Nothing
  ORef{}          -> Nothing
  ONone           -> Nothing

-- | Find object. Return `ONone` in case of can't found it
findObject :: AddressRef -> StWorld Object
findObject addr = do
  (obj, dyns) <- fromMaybe (ONone, []) <$> lookupInMemory addr
  fromMaybe ONone <$> through obj dyns

lookupInMemory :: AddressRef -> StWorld (Maybe (Object, [T.Text]))
lookupInMemory (AddressRef word accessors) =
  (fmap . fmap) (\var -> (var^.rawObjA, accessors)) (getVar word)

-- TODO: Revisar el tema ya que se pasa un AddressRef los dyns
--       se deberian de ver si son innecesarios. En general, estudiar
--       cual debe ser el comportamiento del GC cuando se usan Vec y Object
-- | Drops a variable when its reference counter reaches 0
dropVarWorld :: AddressRef -> StWorld ()
dropVarWorld addrRef = do
  val <- getVar (addrRef^.refA)
  case val of
    Just var -> do
      let var' = var & refCounterA %~ (\ct -> (-) ct 1)
      if var'^.refCounterA == 0 then
        innerStateA.tableA %= IM.delete (fromIntegral $ addrRef^.refA)
      else
        setVar (addrRef^.refA) var'
    Nothing -> throw DropVariableAlreadyDropped

-- | Get a value object
getObject :: Object -> StWorld Object
getObject (ORef word) = findObject (simple word)
getObject obj         = return obj

-- | Follow reference pointer until and not reference object.
-- It throws a exception when reaches the limit 50
follow' :: Word -> StWorld Word
follow' w = follow'' w 50
  where
    follow'' :: Word -> Int -> StWorld Word
    follow'' word times
      | times <= 0 = throw ExcededRecursiveLimit
      | otherwise = do
        obj <- findObject (simple word)
        case obj of
          ORef word' -> follow'' word' (times - 1)
          _          -> return word

follow :: Word -> StWorld Object
follow word = follow' word >>= findObject . simple

-- | Allow execute actions from ScopeM into Interpreter
liftScope :: ScopeM b -> StWorld b
liftScope scopeM = do
  innerState <- get
  scope <- use $ innerStateA.scopeA
  let flattedScope = flatScope scope
  let tempScope = innerStateA .~ (def { _stackScope = [flattedScope] }) $ innerState
  (value, newScope) <- liftIO $ runStateT (runExceptT scopeM) tempScope
  let tempBase = newScope^.innerStateA.currentScopeA
  let newBaseScope = flatScope (Scope tempBase [scope^.currentScopeA])
  innerStateA.scopeA.currentScopeA .= newBaseScope
  case value of
    Right val -> return val
    Left err  -> throw $ ScopeError err
