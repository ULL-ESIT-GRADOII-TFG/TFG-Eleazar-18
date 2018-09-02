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
import           Compiler.Prettify
import           Compiler.Scope
import           Compiler.Types
import           Compiler.Utils


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
  findAddress = liftScope . findAddress
  findAddress' = liftScope . findAddress'

instance Default (World o) where
  def = World
    { _table = mempty
    , _scope = def
    , _lastTokenInfo = def
    }

instance TypeName o => Pretty (World o) where
  pretty (World tb scope _) =
    let aux = leftInnerJoin ((HM.toList (_renameInfo $ _currentScope scope)) & each._2 %~ unAddr . _ref) (IM.toList tb)
    in
      "World {" <> line <>
      -- Do a cross joint
      nest 2 (vcat (map (\(name, address, value) ->
        pretty name
        <+> " -> "
        <+> "#"
        <> pretty (show address)
        <+> pretty value) aux)) <> line <>
      "}"

instance MemoryAccessor StWorld Object where
  type Store StWorld = Rc

  getVar addr = do
    table <- use tableA
    case IM.lookup (unAddr addr) table of
      Just val -> return val
      Nothing  -> throw $ NotFoundObject (unAddr addr)

  setVar addr var = tableA %= IM.insert (unAddr addr) var

  findPathVar (PathVar addr accessors) = through addr accessors

  setPathVar (PathVar addr dyns) var = do
    addr' <- buildFollowingPath addr dyns
    setVar addr' var
    return addr'

instance Deallocate StWorld where
  deleteVar addr = do
    obj <- unwrap <$> getVar addr
    mapM_ deleteVar (innerRefs obj)
    dropVarWorld addr

  deleteUnsafe addr = void $ IM.delete (unAddr addr) <$> use tableA

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
  idName <- getNewId
  let addr = idToAdr idName

  case var ^. rawObjA of
    ONone -> do
      setVar word (var & rawObjA .~ OObject Nothing (HM.singleton acc addr))
      setVar addr (pure obj)
    OObject parent attrs -> do
      setVar word (var & rawObjA .~ OObject parent (HM.insert acc addr attrs))
      setVar addr (pure obj)
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
