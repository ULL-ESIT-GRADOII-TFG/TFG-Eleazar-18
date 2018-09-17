{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compiler.World where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import qualified Data.Text                  as T
import           Data.Text.Prettyprint.Doc
import           Lens.Micro.Platform        hiding (zoom)

import           Compiler.Error
import {-# SOURCE #-} Compiler.Object            ()
-- import           Compiler.Prettify
import           Compiler.Scope
import           Compiler.Types


instance Functor Rc where
  fmap f (Rc c o) = Rc c (f o)

instance Applicative Rc where
  pure = Rc 1
  (<*>) (Rc v1 f) (Rc v2 o) = Rc (max v1 v2) (f o)

instance Wrapper Rc where
  unwrap (Rc _ o) = o

instance TypeName o => Pretty (Rc o) where
  pretty (Rc c o) = "RC" <+> pretty c <+> pretty (typeName o)

instance GetInfo StWorld where
  getInfo = _lastTokenInfo <$> get

instance Naming StWorld where
  newId = liftScope . newId
  getNewId = liftScope getNewId
  findIdPath = liftScope . findIdPath
  findIdPath' = liftScope . findIdPath'

instance Default (World o) where
  def = World
    { _table = mempty
    , _scope = def
    , _lastTokenInfo = def
    , _gc = []
    , _free = []
    , _counterAddr = 0
    , _tableIdPath = mempty
    }

instance TypeName o => Pretty (World o) where
  pretty World{_table=_tb, _scope=_scope} =
    -- let aux = leftInnerJoin ((HM.toList (_renameInfo $ _currentScope scope)) & each._2 %~ unAddr . _ref) (IM.toList tb)
    -- in
      "World {" <> line <>
      -- Do a cross joint
      -- nest 2 (vcat (map (\(name, address, value) ->
      --   pretty name
      --   <+> " -> "
      --   <+> "#"
      --   <> pretty (show address)
      --   <+> pretty value) aux)) <> line <>
      "}"

instance MemoryAccessor StWorld Object where
  type Store StWorld = Rc
  newVar rc = do
    addr <- use counterAddrA
    setVar (Adr addr) rc
    counterAddrA += 1
    return $ Adr addr

  newVarWithName name obj = do
    addr <- newVar $ wrap obj
    idName <- newId name
    linkIdPathToAddressPath idName (simple addr)
    return addr

  linkIdPathToAddressPath idName addrPath = do
    tableIdPathA %= IM.insert (idName^.idVarA & unIdName) (addrPath^.refA)

  unlinkIdPathToAddressPath idName = do
    tableIdPathA %= IM.delete (idName^.idVarA & unIdName)

  getVar addr = do
    table <- use tableA
    case IM.lookup (unAddr addr) table of
      Just val -> return val
      Nothing  -> throw $ NotFoundObject (unAddr addr)

  setVar addr var = tableA %= IM.insert (unAddr addr) var

  findVarWithIdPath idPath = do
    idPathToAddressPath idPath >>= findVarWithAddressPath

  setVarWithIdPath idPath rc = do
    idPathToAddressPath idPath >>= flip setVarWithAddressPath rc

  findVarWithAddressPath (AddressPath addr accessors) = through addr accessors

  setVarWithAddressPath (AddressPath addr dyns) var = do
    addr' <- buildFollowingPath addr dyns
    case unwrap var of
      ORef ref -> do
        ref' <- follow' ref
        if ref' /= addr then do
          setVar addr' var
          return addr'
        else return addr'
      _ -> do
        setVar addr' var
        return addr'

  idPathToAddressPath idPath = do
    tableIdPath <- use tableIdPathA
    case IM.lookup (idPath^.idVarA & unIdName) tableIdPath of
      Just addr -> return (AddressPath addr (idPath^.accessorsA))
      Nothing -> throw $ WorldError ""


instance Deallocate StWorld where
  collectAddr addr = gcA %= (addr:)
  removeLocal = do
    gc <- use gcA
    mapM_ deleteVar gc
    gcA .= []

  deleteVar addr = do
    obj <- unwrap <$> getVar addr
    mapM_ deleteVar (innerRefs obj)
    dropVarWorld addr

  deleteUnsafe addr = tableA %= IM.delete (unAddr addr)

dropVarWorld :: Address -> StWorld Bool
dropVarWorld addrRef = do
  val <- getVar addrRef
  let var' = val & refCounterA %~ (\ct -> (-) ct 1)
  if var'^.refCounterA == 0 then do
    deleteUnsafe addrRef
    return True
  else do
    setVar addrRef var'
    return False

-- | Build objects following a given path `path` from initial address `addr`.
-- It returns last object following path.
-- > obj = {}
-- > obj.foo.bar # No currently possible to do in this way
-- > obj
-- { foo: { bar: ONone } }
buildFollowingPath :: Address -> [T.Text] -> StWorld Address
buildFollowingPath addr path =
  (flip . flip foldM) addr path $ \addrCurrent acc -> do
    nextAddr <- follow' addrCurrent
    obj <- follow nextAddr
    catchError (access obj acc)
      (\err -> case err^.errorInternalA of
        NotPropertyFound{} ->
          addSubObject nextAddr acc ONone
        _ -> throwError err
      )

-- | Get a address and try to add a new accesor to it
addSubObject :: Address -> T.Text -> Object -> StWorld Address
addSubObject word acc obj = do
  var <- do
    lastAddr <- follow' word
    getVar lastAddr `catchError` const (throw NotExtensibleObject)

  addr <- newVar $ wrap obj

  case var ^. rawObjA of
    ONone -> do
      setVar word (var & rawObjA .~ OObject Nothing (HM.singleton acc addr))
    OObject parent attrs -> do
      setVar word (var & rawObjA .~ OObject parent (HM.insert acc addr attrs))
    _ -> throw NotExtensibleObject
  return addr

-- | Access through a path accessors
through :: Address -> [T.Text] -> StWorld (Rc Object, Address)
through addr accessors = do
  addr' <- foldM (\lastAddr accessor -> do
    obj <- follow lastAddr
    obj `access` accessor
    ) addr accessors
  var <- getVar addr'
  return (var, addr')

-- TODO: This must to be explained in detail why one is going to be better than other
--
-- | Allow execute actions from ScopeM into Interpreter
liftScope :: ScopeM b -> StWorld b
liftScope scopeM = do
  scope <- use scopeA
  let flattedScope = flatScope scope
  let tempScope    = def { _stackScope = [flattedScope] }
  (value, newScope) <- liftIO $ runStateT (runExceptT scopeM) tempScope
  let tempBase     = newScope ^. currentScopeA
  let newBaseScope = flatScope (Scope tempBase [scope ^. currentScopeA])
  scopeA . currentScopeA .= newBaseScope
  case value of
    Right val -> return val
    Left  err -> throwWithInfo (err^.errorInfoA) $ ScopeError (err^.errorInternalA)

liftScope' :: ScopeM b -> StWorld b
liftScope' scopeM = do
  scope <- use scopeA
  (value, newScope) <- liftIO $ runStateT (runExceptT scopeM) scope
  scopeA .= newScope
  case value of
    Right val -> return val
    Left  err -> throwWithInfo (err^.errorInfoA) $ ScopeError (err^.errorInternalA)
