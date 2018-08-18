{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Compiler.World where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Text.Prettyprint.Doc
import           Lens.Micro.Platform

import           Compiler.Error
import           Compiler.Object
import           Compiler.Parser.Types
import           Compiler.Prelude.Utils
import           Compiler.Prettify
import           Compiler.Scope
import           Compiler.Types
import           Compiler.Utils


data Rc o = Rc
  { _refCounter :: !Address
  , _rawObj     :: !o
  }
  deriving Show

makeSuffixLenses ''Rc

instance Functor Rc where
  fmap f (Rc c o) = Rc c (f o)

instance Applicative Rc where
  pure = Rc 1
  (<*>) (Rc v1 f) (Rc v2 o) = Rc (max v1 v2) (f o)

instance Wrapper Rc where
  unwrap (Rc _ o) = o

instance TypeName o => Prettify (Rc o) where
  prettify (Rc c o) _verbose = "RC" <+> pretty c <+> pretty (typeName o)

type StWorld = StateT World (ExceptT (ErrorInfo WorldError) IO)

-- | Keeps all information of running program (memory, debugging info ...)
data World = World
  { _table         :: IM.IntMap (Rc (Object StWorld))
  -- ^ Generic table to storage all vars/objects
  , _scope         :: Scope
  -- ^ Root Scope.
  , _lastTokenInfo :: TokenInfo
  -- ^ Used to generate precise errors locations
  }

makeSuffixLenses ''World

instance GetInfo StWorld where
  getInfo = _lastTokenInfo <$> get

instance Naming StWorld where
  newId = liftScope . newId
  getNewId = liftScope getNewId
  findAddress = liftScope . findAddress

instance Default World where
  def = World
    { _table = mempty
    , _scope = def
    , _lastTokenInfo = def
    }

instance Prettify World where
  prettify (World tb scope _) verbose =
    let aux = leftInnerJoin ((M.toList (_renameInfo $ _currentScope scope)) & each._2 %~ _ref) (IM.toList tb)
    in
      "World {" <> line <>
      -- Do a cross joint
      nest 2 (vcat (map (\(name, address, value) ->
        pretty name
        <+> " -> "
        <+> "#"
        <> pretty (show address)
        <+> prettify value verbose) aux)) <> line <>
      "}"

instance MemoryManagement StWorld where
  type Store StWorld = Rc
  type RawObj StWorld = Object StWorld

  getVar addr = do
    table <- use tableA
    case IM.lookup addr table of
      Just val -> return val
      Nothing  -> throw $ NotFoundObject addr


  setVar addr var = tableA %= IM.insert addr var

  deleteVar _addr = return False -- TODO: undefined

  deleteUnsafe addr = void $ IM.delete addr <$> use tableA

  --findPathVar :: PathVar -> mm (Store mm (RawObj mm), Address)
  findPathVar (PathVar _addr _accessors) = undefined -- TODO do
    -- obj <- unwrap <$> getVar addr
    -- through obj accessors

  setPathVar (PathVar addr dyns) var = do
    addr' <- buildFollowingPath addr dyns
    setVar addr' var

-- | Build objects following a given path `path` from initial address `addr`.
-- It returns last object following path.
-- > obj = {}
-- > obj.foo.bar # No currently possible to do in this way
-- > obj
-- { foo: { bar: ONone } }
buildFollowingPath :: Address -> [T.Text] -> StWorld Address
buildFollowingPath addr path =
  (flip . flip foldM) addr path $ \addrCurrent acc -> do
    lastRef <- follow' addrCurrent
    on (ORef lastRef) acc
    --  Nothing    -> addObjectToObject lastRef acc ONone -- TODO

-- MOVE TO: ObjectOperations
-- | Like `on` but doesn't perform a internal search or class search. Its mainly used to
-- modify objects not to access them
on :: Object StWorld -> T.Text -> StWorld Address
on obj acc = case obj of
  OObject mClassId dicObj -> (maybe (throw $ NotFoundObject 0) return) =<< (attemps
    -- Local search
    [
      case M.lookup acc dicObj of
        Just addr -> Just <$> follow' addr
        Nothing   -> return Nothing

    , do
      memory <- use tableA
      return $
        (do
          clsId <- mClassId
          cls <- unwrap <$> IM.lookup clsId memory
          case cls of
            OClassDef _ _ attrs ->
              (M.lookup acc attrs)
              <|>
              -- Find into share methods (operators) too
              (do
                (_, _, name) <- M.lookup acc operatorsPrecedence
                M.lookup name attrs
              )
            _ -> Nothing
        )

    -- Internal search
    , return $ getMethods obj acc
    ])
  ORef addr        -> follow addr >>= (`on` acc)
  _                -> throw $ NotFoundObject 0

-- TODO Reorganize to add internal docs
getMethods :: Object StWorld -> T.Text -> Maybe Address
getMethods _obj _name = undefined -- TODO Remove

-- | Get a address and try to add a new accesor to it
addObjectToObject :: Address -> T.Text -> Object StWorld -> StWorld Address
addObjectToObject word acc obj = do
  var <- do
    lastAddr <- follow' word
    getVar lastAddr `catchError` const (throw NotExtensibleObject)
  addr <- getNewId

  case var ^. rawObjA of
    ONone -> do
      setVar word (var & rawObjA .~ OObject Nothing (M.singleton acc addr))
      setVar addr (pure obj)
    OObject parent attrs -> do
      setVar word (var & rawObjA .~ OObject parent (M.insert acc addr attrs))
      setVar addr (pure obj)
    _ -> throw NotExtensibleObject
  return addr

lookupInMemory :: PathVar -> StWorld (Object StWorld, [T.Text])
lookupInMemory (PathVar addr accessors) = do
  obj <- unwrap <$> getVar addr
  return (obj, accessors)

-- | Access through a path accessors
through :: Object StWorld -> [T.Text] -> StWorld (Object StWorld)
through = foldM (\obj acc -> obj `on` acc >>= follow)

-- | Return the first `Just` get from list else try next
attemps :: Monad m => [m (Maybe a)] -> m (Maybe a)
attemps [] = return Nothing
attemps (x:xs) = do
  val <- x
  case val of
    Just a  -> return $ Just a
    Nothing -> attemps xs

-- TODO: This must to be explained in detail why one is going to be better than other
--
-- | Allow execute actions from ScopeM into Interpreter
liftScope :: ScopeM b -> StWorld b
liftScope scopeM = do
  scope      <- use scopeA
  let flattedScope = flatScope scope
  let tempScope = def { _stackScope = [flattedScope] }
  (value, newScope) <- liftIO $ runStateT (runExceptT scopeM) tempScope
  let tempBase     = newScope ^. currentScopeA
  let newBaseScope = flatScope (Scope tempBase [scope ^. currentScopeA])
  scopeA.currentScopeA .= newBaseScope
  case value of
    Right val -> return val
    Left  err -> throw $ ScopeError err
