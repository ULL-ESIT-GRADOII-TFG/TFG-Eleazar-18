{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compiler.Object where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as HM
import           Data.Scientific
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector                   as V
import qualified Data.Yaml                     as Y
import           Lens.Micro.Platform
import           Text.Regex.PCRE.Light

import           Compiler.Error
import qualified Compiler.Object.OBool         as OB
import qualified Compiler.Object.ODouble       as OD
import qualified Compiler.Object.ONum          as ON
import qualified Compiler.Object.ORegex        as OR
import qualified Compiler.Object.OShellCommand as OC
import qualified Compiler.Object.OStr          as OS
import qualified Compiler.Object.OVector       as OV
import           Compiler.Prelude.Utils
import           Compiler.Prettify
import           Compiler.Types
import           Compiler.World                ()


instance ToObject Y.Value where
  toObject yamlType = case yamlType of
    Y.Object hashmap   -> do
      oMap <- mapM (\el -> do
                       obj <- toObject el
                       newVar $ pure obj
                   ) hashmap
      return $ OObject Nothing oMap
    Y.Array vector     -> toObject vector
    Y.String text      -> toObject text
    Y.Number number -> return $ case floatingOrInteger number of
      Left double -> ODouble double
      Right num   -> ONum num
    Y.Bool bool        -> toObject bool
    Y.Null             -> return ONone

instance FromObject Y.Value where
  fromObject obj = case obj of
    OStr text -> return $ Y.String text
    OBool bool -> return $ Y.Bool bool
    ODouble double -> return . Y.Number $ fromFloatDigits double
    ONum num -> return . Y.Number $scientific (toInteger num) 0
    OVector vector -> do
      vec <- mapM (\ref -> do
                      obj' <- unwrap <$> getVar ref
                      fromObject obj'
                  ) vector
      return $ Y.Array vec
    OObject _ hashmap -> do
      hashmap' <- mapM (\ref -> do
                           obj' <- unwrap <$> getVar ref
                           fromObject obj'
                       ) hashmap
      return $ Y.Object hashmap'
    ONone -> return Y.Null
    o -> throw $ NotImplicitConversion (typeName o) "JSON_Value"


instance ToObject Object where
  toObject = return

instance ToObject a => ToObject (StWorld a) where
  toObject mObj = do
    obj <- mObj
    toObject obj

instance FromObject Object where
  fromObject = return

instance ToObject a => ToObject (IO a) where
  toObject io = do
    obj <- liftIO io
    toObject obj

instance ToObject () where
  toObject () = return ONone

instance FromObject () where
  fromObject ONone = return ()
  fromObject o     = throw $ NotImplicitConversion (typeName o) "None"

instance ToObject Bool where
  toObject = return . OBool

instance FromObject Bool where
  fromObject (OBool bool) = return bool
  fromObject o            = throw $ NotImplicitConversion (typeName o) "Bool"

instance ToObject Int where
  toObject = return . ONum

instance FromObject Int where
  fromObject (ONum num) = return num
  fromObject o          = throw $ NotImplicitConversion (typeName o) "Num"

instance ToObject Double where
  toObject = return . ODouble

instance FromObject Double where
  fromObject (ODouble num) = return num
  fromObject o             = throw $ NotImplicitConversion (typeName o) "Double"

instance ToObject a =>
    ToObject (V.Vector a) where
  toObject vec = do
    elems <- V.mapM (\el -> do
                        obj <- toObject el
                        newVar $ pure obj
                    ) vec
    return $ OVector elems

instance FromObject a =>
    FromObject (V.Vector a) where
  fromObject (OVector vec) = mapM (\ref -> do
                                      obj <- unwrap <$> getVar ref
                                      fromObject obj
                                  ) vec
  fromObject o = throw $ NotImplicitConversion (typeName o) "Vector"

instance ToObject a => ToObject [a] where
  toObject = toObject . V.fromList

instance FromObject a => FromObject [a] where
  fromObject (OVector vec) = V.toList <$> mapM (\ref -> do
                                                   obj <- unwrap <$> getVar ref
                                                   fromObject obj
                                               ) vec
  fromObject o = throw $ NotImplicitConversion (typeName o) "Vector"

instance ToObject a => ToObject (HM.HashMap T.Text a) where
  toObject mmap = do
    oMap <- mapM (\el -> do
                     obj <- toObject el
                     newVar $ pure obj
                 ) mmap
    return $ OObject Nothing oMap

instance FromObject a => FromObject (HM.HashMap T.Text a) where
  fromObject (OObject _ dic) =
    mapM (\ref -> do
             obj <- unwrap <$> getVar ref
             fromObject obj
         ) dic
  fromObject o = throw $ NotImplicitConversion (typeName o) "Object"

instance ToObject Char where
  toObject = return . OStr . T.singleton

instance FromObject Char  where
  fromObject (OStr text) | T.length text == 1 = return (T.head text)
                         | otherwise = throw $ WorldError "Expect a single char string"
  fromObject o           = throw $ NotImplicitConversion (typeName o) "Char"


instance {-# OVERLAPPING #-} ToObject [Char] where
  toObject = return . OStr . T.pack

instance {-# OVERLAPPING #-} FromObject [Char] where
  fromObject (OStr text) = return (T.unpack text)
  fromObject o           = throw $ NotImplicitConversion (typeName o) "Str"

instance ToObject B.ByteString where
  toObject = return . OStr . TE.decodeUtf8

instance FromObject B.ByteString where
  fromObject (OStr text) = return $ TE.encodeUtf8 text
  fromObject o           = throw $ NotImplicitConversion (typeName o) "Str"

instance FromObject ShellType where
  fromObject (OShellCommand text) = return $ ShellType text
  fromObject o = throw $ NotImplicitConversion (typeName o) "ShellCommand"

instance ToObject T.Text where
  toObject = return . OStr

instance FromObject T.Text where
  fromObject (OStr text) = return text
  fromObject o           = throw $ NotImplicitConversion (typeName o) "Str"

instance ToObject Regex where
  toObject = return . ORegex

instance FromObject Regex where
  fromObject (ORegex regex) = return regex
  fromObject o              = throw $ NotImplicitConversion (typeName o) "Regex"

instance FromObject a => FromObject (Maybe a) where
  fromObject ONone = return Nothing
  fromObject obj   = Just <$> fromObject obj

instance ToObject a => ToObject (Maybe a) where
  toObject = maybe (return ONone) toObject


instance Callable StWorld Object where
  -- | From memory address, check if object callable and call it with given arguments
  call pathVar args = do
    objCaller <- unwrap <$> getVar (pathVar^.refA)
    (obj, _address) <- findPathVar pathVar
    let obj' = unwrap obj
    if null (pathVar ^. dynPathA) then
      directCall obj' args
    else
      directCall obj' (objCaller:args)

  directCall obj objs = case obj of
    OFunc _ ids prog ->
      if length ids /= length objs then
        throw $ NumArgsMissmatch (length ids) (length objs)
      else
        runProgram $ prog objs

    ONative native ->
      native objs

    OBound self method ->
      directCall (ORef method) (ORef self:objs)

    OClassDef _name refCls methods -> do
      self <- newVar . wrap $ OObject (Just refCls) mempty
      case HM.lookup "__init__" methods of
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

instance Iterable StWorld Object where
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
          case HM.lookup "__map__" methods of
            Just func' -> do
              func'' <- follow func'
              directCall func'' [obj, ONative $ \obs -> func (head obs)]
            Nothing -> return ONone
        o -> throw $ NotIterable (typeName o)
    o -> throw $ NotIterable (typeName o)

instance Booleanable StWorld Object where
  -- | Check truthfulness of an object
  checkBool obj = case obj of
    OBool bool                     -> return bool
    OObject (Just classRef) _attrs -> do
      clsObj <- unwrap <$> getVar classRef
      case clsObj of
        OClassDef _name _ref methods -> case HM.lookup "__bool__" methods of
          Just func' -> do
            func'' <- follow func'
            directCall func'' [obj] >>= checkBool
          Nothing -> throw $ NotBoolean "Object"
        o -> throw $ NotBoolean (typeName o)
    _ -> throw $ NotBoolean (typeName obj)

instance Showable StWorld Object where
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
    OFunc _env _args _body -> return "Function"
      -- body' <- prettify verbose (body (repeat ONone))
      -- return
      --   $   "Function with args:"
      --   <+> pretty args
      --   <>  "{"
      --   <>  line
      --   <>  nest 2 body'
      --   <>  line
      --   <>  "}"
    ONative _          -> return "Native Function"
    OClassDef name _ _ -> return $ "class" <+> pretty name
    OObject _ methods  -> do
      dic <-
        mapM
            (\(key, obj') -> do
              objDoc <- showObject (ORef obj')

              return $ pretty key <+> "->" <+> objDoc
            )
          $ HM.toList methods
      return $ "{" <> line <> nest 2 (vcat dic) <> line <> "}"
    ONativeObject _ -> return "Native Object"

      -------------------------------------------------------

instance GetRef StWorld Object where
  mkRef (PathVar addr accessors) =
    if accessors == [] then do
      let ref = ORef addr
      addr' <- newVar $ wrap ref
      return (ref, addr')
    else do
      let (initials, rest) = splitAt (length accessors - 1) accessors
      (rcParent, addrParent) <- findPathVar (PathVar addr initials)
      (rcChild, addrChild) <- findPathVar (PathVar addrParent rest)
      setVar addrChild (rcChild & refCounterA %~ (+1))
      case (unwrap rcParent, unwrap rcChild) of
        (OObject{}, OFunc{} ) -> do
          let boundMethod = OBound addrParent addrChild
          setVar addrParent (rcParent & refCounterA %~ (+1))
          address <- newVar $ wrap boundMethod
          return (boundMethod, address)
        (_, _) -> do
          let ref = ORef addrChild
          addr' <- newVar $ wrap ref
          return (ref, addr')

instance Redirection StWorld where
  -- | Follow reference pointer until and not reference object.
  -- It throws a exception when reaches the limit 50
  follow' w = follow'' w 50
    where
      follow'' :: Address -> Int -> StWorld Address
      follow'' word times
        | times <= 0 = throw ExcededRecursiveLimit
        | otherwise = do
          obj <- unwrap <$> getVar word
          case obj of
            ORef word' -> follow'' word' (times - 1)
            _          -> return word

instance AccessHierarchy StWorld Object where
    -- zoom :: RawObj mm -> T.Text -> mm Address
  access obj acc = case obj of
    OObject mClassId dicObj -> attemps
      -- Local search
      [
        case HM.lookup acc dicObj of
          Just addr -> follow' addr
          Nothing   -> notFound

      , do
        clsId <- maybe notFound return mClassId
        cls <- unwrap <$> getVar clsId
        case cls of
          OClassDef _ _ attrs -> maybe notFound return (
            HM.lookup acc attrs
            <|>
            -- Find into share methods (operators) too
            (do
              (_, _, name) <- HM.lookup acc operatorsPrecedence
              HM.lookup name attrs
            ))
          _ -> notFound

      -- Internal search
      , do
        methods <- internalMethods obj
        case HM.lookup acc methods of
          Just addr -> return addr
          Nothing   -> notFound
      ]
    ORef addr        -> follow addr >>= (`access` acc)
    _                -> do
      methods <- internalMethods obj
      case HM.lookup acc methods of
        Just addr -> return addr
        Nothing   -> notFound
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

instance GetInnerRefs Object where
  innerRefs obj = case obj of
    OVector       vec     -> V.toList vec
    OBound addr1 addr2    -> [addr1, addr2]
    ORef rfs              -> [rfs]
    OFunc env _args _body -> HM.elems env
    OObject _ methods     -> HM.elems methods
    _                     -> []

instance TypeName Object where
  typeName obj = case obj of
    OClassDef{}     -> "ClassDef"
    ONative{}       -> "NativeFunction"
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
    ONativeObject{} -> "NativeObject"

instance Prettify Object where
  prettify obj _verbose = pretty $ typeName obj

internalMethods :: Object -> StWorld (HM.HashMap T.Text Address)
internalMethods obj = case obj of
  OStr{}          -> mkMethods OS.methods
  OBool{}         -> mkMethods OB.methods
  ODouble{}       -> mkMethods OD.methods
  ONum{}          -> mkMethods ON.methods
  OVector{}       -> mkMethods OV.methods
  ORegex{}        -> mkMethods OR.methods
  OShellCommand{} -> mkMethods OC.methods
  OFunc{}         -> return mempty -- TODO: Add methods mappend -> monoid instance
  ONative{}       -> return mempty
  OBound{}        -> return mempty
  OObject{}       -> return mempty -- TODO: any string should return ONone
  OClassDef{}     -> return mempty
  ORef{}          -> return mempty
  ONone           -> return mempty
  ONativeObject{} -> return mempty
  where
    mkMethods
      :: [(T.Text, [Object] -> StWorld Object)]
      -> StWorld (HM.HashMap T.Text Address)
    mkMethods methods =
      HM.fromList
        <$> mapM
              (\(name, func) -> do
                ref <- newVarWithName name (ONative func)
                return (name, ref)
              )
              methods
