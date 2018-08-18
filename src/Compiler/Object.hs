{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Compiler.Object where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.ByteString           as B
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector               as V
import           Lens.Micro.Platform       hiding (zoom)
import           Text.Regex.PCRE.Light

import           Compiler.Error
-- import qualified Compiler.Prelude.OVector  as OV
import           Compiler.Prelude.Utils
import           Compiler.Types


data Object mm
  = OStr T.Text
  | OBool Bool
  | ODouble Double
  | ONum Int
  | ORegex Regex
  -- ^ Regex expression following PCRE syntax
  | OShellCommand T.Text
  -- ^ Shell command
  | OVector (V.Vector Address)
  -- ^ Sequence of objects
  | OFunc (M.Map T.Text Address) [Address] ([Object mm] -> Prog mm mm (Object mm))
  -- ^ Lambda with possible scope/vars attached
  | OBound Address Address
  -- ^ Used to Bound methods to variables
  | OObject (Maybe Address) (M.Map T.Text Address)
  -- ^ Object instance from class Address
  | ONative ([Object mm] -> Prog mm mm (Object mm))
  -- ^ Native object
  | ORef Address
  -- ^ Pointer reference
  | OClassDef
    { nameClass       :: T.Text
    , refClass        :: Address
    , attributesClass :: M.Map T.Text Address
    }
  | ONone

class MemoryManagement mm => ToObject o mm where
  toObject :: o -> mm (Object mm)

class MemoryManagement mm => FromObject o mm where
  fromObject :: Object mm -> mm o

instance MemoryManagement mm => ToObject (Object mm) mm where
  toObject = return

instance MemoryManagement mm => FromObject (Object mm) mm where
  fromObject = return

instance MemoryManagement mm => ToObject () mm where
  toObject () = return ONone

instance MemoryManagement mm => FromObject () mm where
  fromObject ONone = return ()
  fromObject _     = throw NotImplicitConversion

instance MemoryManagement mm => ToObject Bool mm where
  toObject = return . OBool

instance MemoryManagement mm => FromObject Bool mm where
  fromObject (OBool bool) = return bool
  fromObject _            = throw NotImplicitConversion

instance MemoryManagement mm => ToObject Int mm where
  toObject = return . ONum

instance MemoryManagement mm => FromObject Int mm where
  fromObject (ONum num) = return num
  fromObject _          = throw NotImplicitConversion

instance MemoryManagement mm => ToObject Double mm where
  toObject = return . ODouble

instance MemoryManagement mm => FromObject Double mm where
  fromObject (ODouble num) = return num
  fromObject _             = throw NotImplicitConversion

instance (MemoryManagement mm, ToObject a mm, Object mm ~ RawObj mm) =>
    ToObject (V.Vector a) mm where
  toObject vec = do
    elems <- V.mapM (\el -> do
                        obj <- toObject el
                        newVar $ pure obj
                    ) vec
    return $ OVector elems

instance (MemoryManagement mm, FromObject a mm, Object mm ~ RawObj mm) =>
    FromObject (V.Vector a) mm where
  fromObject (OVector vec) = mapM (\ref -> do
                                      obj <- unwrap <$> getVar ref
                                      fromObject obj
                                  ) vec
  fromObject _             = throw NotImplicitConversion

instance (MemoryManagement mm, ToObject a mm, Object mm ~ RawObj mm) => ToObject [a] mm where
  toObject = toObject . V.fromList

instance (MemoryManagement mm, FromObject a mm, Object mm ~ RawObj mm) => FromObject [a] mm where
  fromObject (OVector vec) = V.toList <$> mapM (\ref -> do
                                                   obj <- unwrap <$> getVar ref
                                                   fromObject obj
                                               ) vec
  fromObject _             = throw NotImplicitConversion

instance (MemoryManagement mm, ToObject a mm, Object mm ~ RawObj mm) => ToObject (M.Map T.Text a) mm where
  toObject mmap = do
    oMap <- mapM (\el -> do
                     obj <- toObject el
                     newVar $ pure obj
                 ) mmap
    return $ OObject Nothing oMap

instance (MemoryManagement mm, FromObject a mm, Object mm ~ RawObj mm) => FromObject (M.Map T.Text a) mm where
  fromObject (OObject _ dic) =
    mapM (\ref -> do
             obj <- unwrap <$> getVar ref
             fromObject obj
         ) dic
  fromObject _               = throw NotImplicitConversion

-- There aren't a char type equivalent right now. Remind: Two paths [Char] String
-- instance ToObject Char where
--   toObject = OStr . T.singleton

instance {-# OVERLAPPING #-} MemoryManagement mm => ToObject [Char] mm where
  toObject = return . OStr . T.pack

instance {-# OVERLAPPING #-} MemoryManagement mm => FromObject [Char] mm where
  fromObject (OStr text) = return (T.unpack text)
  fromObject _           = throw NotImplicitConversion

instance MemoryManagement mm => ToObject B.ByteString mm where
  toObject = return . OStr . TE.decodeUtf8

instance MemoryManagement mm => FromObject B.ByteString mm where
  fromObject (OStr text) = return $ TE.encodeUtf8 text
  fromObject _           = throw NotImplicitConversion

instance MemoryManagement mm => ToObject T.Text mm where
  toObject = return . OStr

instance MemoryManagement mm => FromObject T.Text mm where
  fromObject (OStr text) = return text
  fromObject _           = throw NotImplicitConversion

instance MemoryManagement mm => ToObject Regex mm where
  toObject = return . ORegex

instance FromObject a mm => FromObject (Maybe a) mm where
  fromObject ONone = return Nothing
  fromObject obj   = Just <$> fromObject obj

instance (MemoryManagement mm, ToObject a mm) => ToObject (Maybe a) mm where
  toObject = maybe (return ONone) toObject

instance MemoryManagement mm => FromObject Regex mm where
  fromObject (ORegex regex) = return regex
  fromObject _              = throw NotImplicitConversion

instance
  ( InstructionsLike mm
  , MemoryManagement mm
  , Wrapper (Store mm)
  , MonadTrans (Prog mm)
  , Object mm ~ RawObj mm)
  => ObjectOperations mm (Object mm) where

  -- | From memory address, check if object callable and call it with given arguments
  call pathVar args = do
    (obj, _address) <- findPathVar pathVar
    let obj' = unwrap obj
    if null (pathVar ^. dynPathA) then
      directCall obj' args
    else
      directCall obj' (obj':args)

  directCall obj objs = case obj of
    OFunc _ ids prog ->
      if length ids /= length objs then
        throw $ NumArgsMissmatch (length ids) (length objs)
      else
        runProgram $ prog objs

    ONative native ->
      runProgram $ native objs

    OBound self method ->
      directCall (ORef method) (ORef self:objs)

    OClassDef _name refCls methods -> do
      self <- newVar . wrap $ OObject (Just refCls) mempty
      case M.lookup "__init__" methods of
        Just method -> do
          _ <- directCall (ORef method) (ORef self : objs)
          obj' <- follow self
          deleteUnsafe self
          return obj'
        Nothing ->
          if null objs then do
            obj' <- follow self
            deleteUnsafe self
            return obj'
          else
            throw $ NumArgsMissmatch 0 (length objs)

    ORef ref' -> do
      obj' <- follow ref'
      directCall obj' objs

    t -> throw $ NotCallable (typeName t)

  -- | Iterate over a object if it is iterable
  mapOver obj func = case obj of
    OStr str -> do
      -- TODO: Avoid unpack. Revisit what kind of problems there are to it doesn't exist an
      --       instance of foldable for Text
      mapM_ (func . OStr . T.singleton) (T.unpack str)
      return ONone
    OVector vec                    -> mapM_ (follow >=> func) vec >> return ONone
    ORef    word                   -> follow word >>= flip mapOver func
    OObject (Just classRef) _attrs -> do
      clsObj <- unwrap <$> getVar classRef
      case clsObj of
        OClassDef _name _ref methods ->
          case M.lookup "__map__" methods of
            Just func' -> do
              func'' <- follow func'
              directCall func'' [obj, ONative $ \obs -> lift $ func (head obs)]
            Nothing -> return ONone
        o -> throw $ NotIterable (typeName o)
    o -> throw $ NotIterable (typeName o)

  -- | Check truthfulness of an object
  checkBool obj = case obj of
    OBool bool                     -> return bool
    OObject (Just classRef) _attrs -> do
      clsObj <- unwrap <$> getVar classRef
      case clsObj of
        OClassDef _name _ref methods -> case M.lookup "__bool__" methods of
          Just func' -> do
            func'' <- follow func'
            directCall func'' [obj] >>= checkBool
          Nothing -> throw $ NotBoolean "Object"
        o -> throw $ NotBoolean (typeName o)
    _ -> throw $ NotBoolean (typeName obj)

  showObject obj = case obj of
    OStr          str  -> return $ "\"" <> pretty str <> "\""
    ORegex        _str -> undefined -- return $ "/" ++ T.unpack str ++ "/"
    OShellCommand str  -> return $ "$ " <> pretty str
    ODouble       val  -> return $ pretty val
    OBool         val  -> return $ pretty val
    ONum          val  -> return $ pretty val
    OVector       vec  -> do
      vec' <- V.mapM (follow >=> showObject) vec
      return . list $ V.toList vec'
    OBound _addr1 _addr2 -> return "Bound Method"
    ORef rfs -> do
      obj' <- follow rfs
      showObject obj' <&> ("*" <>) -- TODO: Remove when the project turn it more stable
    ONone                -> return "None"
    OFunc _env args body -> do
      body' <- showInstructions (body (repeat ONone))
      return
        $   "Function with args:"
        <+> pretty args
        <>  "{"
        <>  line
        <>  nest 2 body'
        <>  line
        <>  "}"
    ONative _          -> return "Native Function"
    OClassDef name _ _ -> return $ "class" <+> pretty name
    OObject _ methods  -> do
      dic <-
        mapM
            (\(key, obj') -> do
              objDoc <- showObject (ORef obj')
              return $ pretty key <+> "->" <+> objDoc
            )
          $ M.toList methods
      return $ "{" <> line <> nest 2 (vcat dic) <> line <> "}"

      -------------------------------------------------------

  -- redirect :: Address -> mm Address
  redirect w = follow'' w 50
    where
      follow''
        :: (MemoryManagement mm, MonadError (ErrorInfo WorldError) mm, RawObj mm ~ (Object mm))
        => Address
        -> Int
        -> mm Address
      follow'' word times
        | times <= 0 = throw ExcededRecursiveLimit
        | otherwise = do
          obj <- unwrap <$> getVar word
          case obj of
            ORef word' -> follow'' word' (times - 1)
            _          -> return word

    -- zoom :: RawObj mm -> T.Text -> mm Address
  zoom obj acc = case obj of
    OObject mClassId dicObj -> attemps
      -- Local search
      [
        case M.lookup acc dicObj of
          Just addr -> redirect addr
          Nothing   -> notFound

      , do
        clsId <- maybe notFound return mClassId
        cls <- unwrap <$> getVar clsId
        case cls of
          OClassDef _ _ attrs -> maybe notFound return (
            M.lookup acc attrs
            <|>
            -- Find into share methods (operators) too
            (do
              (_, _, name) <- M.lookup acc operatorsPrecedence
              M.lookup name attrs
            ))
          _ -> notFound

      -- Internal search
      , return $ undefined obj acc -- TODO getMethods
      ]
    ORef addr        -> follow addr >>= (`zoom` acc)
    _                -> throw $ NotFoundObject 0
    where
      notFound = throw $ NotPropertyFound [] acc []

      -- | Return the first `Just` get from list else try next
      -- attemps :: MonadError (ErrorInfo WorldError) m => [m a] -> m a
      attemps [] = notFound
      attemps (x:xs) =
        catchError x ((\case
          NotFoundObject{} -> attemps xs
          NotPropertyFound{} -> attemps xs
          err -> throw err) . _errorInternal)

--getMethods = undefined
-- methods =
--   mapM (\(name, func) ->
--       newVarWithName name (wrap . ONative . lift . func)
--   ) OV.methods



instance TypeName (Object mm) where
  typeName obj = case obj of
    OClassDef{}     -> "ClassDef"
    ONative{}       -> "Native"
    OFunc{}         -> "Function"
    OStr{}          -> "Str"
    ORegex{}        -> "Regex"
    OBound{}        -> "BoundObject"
    OShellCommand{} -> "ShellCommand"
    ODouble{}       -> "Double"
    OBool{}         -> "Bool"
    ONum{}          -> "Num"
    OVector{}       -> "Vector"
    ORef{}          -> "Ref"
    ONone           -> "None"
    OObject{}       -> "Object"

-- | Follow reference pointer until and not reference object.
-- It throws a exception when reaches the limit 50
follow'
  :: ( MemoryManagement mm
     , MonadError (ErrorInfo WorldError) mm
     , RawObj mm ~ (Object mm)
     )
  => Address
  -> mm Address
follow' w = follow'' w 50
  where
    follow''
      :: ( MemoryManagement mm
         , MonadError (ErrorInfo WorldError) mm
         , RawObj mm ~ (Object mm)
         )
      => Address
      -> Int
      -> mm Address
    follow'' word times
      | times <= 0 = throw ExcededRecursiveLimit
      | otherwise = do
        obj <- unwrap <$> getVar word
        case obj of
          ORef word' -> follow'' word' (times - 1)
          _          -> return word

-- | Assure to an Object different from a `ORef`
follow
  :: ( MemoryManagement mm
     , MonadError (ErrorInfo WorldError) mm
     , RawObj mm ~ (Object mm)
     )
  => Address
  -> mm (Object mm)
follow word = do
  word' <- follow' word
  unwrap <$> getVar word'
