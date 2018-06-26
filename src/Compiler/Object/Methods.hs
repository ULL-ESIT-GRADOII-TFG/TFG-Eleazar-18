{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Object.Methods where

import           Control.Monad.Trans.Free
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Text.PrettyPrint

import           Compiler.Error
import           Compiler.Instruction.Utils
import           Compiler.Prelude.Types
import           Compiler.Types
import           Compiler.World.Methods


-- | From memory address, check if object callable and call it with given arguments
callObject :: AddressRef -> [Object] -> StWorld Object
callObject address args = do
  lookupObj <- lookupInMemory address
  case lookupObj of
    Just (obj, accessors) -> do
      mObj <- through obj accessors
      let args' = if null accessors then args else obj:args
      case mObj of
        Just obj' ->
          if null accessors then
            callObjectDirect obj' args'
          else
            catchArgsMethodsError $ callObjectDirect obj' args'
        Nothing   -> throw $ NotPropertyFound [] (T.intercalate "." (address^.dynPathA)) []
    Nothing -> throw $ NotFoundObject (address^.refA)

callObjectDirect :: Object -> [Object] -> StWorld Object
callObjectDirect obj objs = case obj of
  OFunc _ ids prog ->
    if length ids /= length objs then
      throw $ NumArgsMissmatch (length ids) (length objs)
    else
      runProgram $ prog objs

  ONative native -> runProgram $ native objs

  OClassDef _name refCls methods -> do
      self <- newObject $ OObject (Just refCls) mempty
      case M.lookup "__init__" methods of
        Just method -> do
          _ <- catchArgsMethodsError $ callObjectDirect (ORef method) (ORef self:objs)
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
    callObjectDirect obj' objs

  t -> throw $ NotCallable (typeName t)

-- | Iterate over a object if it is iterable
mapObj :: Object -> (Object -> StWorld Object) -> StWorld Object
mapObj obj func = case obj of
    OStr str    -> do
      -- TODO: Avoid unpack. Revisit what kind of problems there are to no exist an
      --      instance of foldable for Text
      mapM_ (func . OStr . T.singleton) (T.unpack str)
      return ONone
    OVector vec -> mapM_ func vec >> return ONone
    ORef word   -> follow word >>= flip mapObj func
    OObject (Just classRef) _attrs -> do
      clsObj <- findObject (simple classRef)
      case clsObj of
        OClassDef _name _ref methods ->
          case M.lookup "__map__" methods of
            Just func' -> do
              func'' <- follow func'
              callObjectDirect func'' [obj, ONative $ normalize func]
            Nothing    -> return ONone
        o  -> throw $ NotIterable (typeName o)
    o -> throw $ NotIterable (typeName o)

-- | Check truthfulness of an object
checkBool :: Object -> StWorld Bool
checkBool obj = case obj of
  OBool bool -> return bool
  OObject (Just classRef) _attrs -> do
    clsObj <- findObject (simple classRef)
    case clsObj of
      OClassDef _name _ref methods ->
        case M.lookup "__bool__" methods of
          Just func' -> do
            func'' <- follow func'
            callObjectDirect func'' [obj] >>= checkBool
          Nothing    -> throw $ NotBoolean "Object"
      o  -> throw $ NotBoolean (typeName o)
  _ -> throw $ NotBoolean (typeName obj)

-- | String representation of objects in REPL
showObject :: Object -> StWorld Doc
showObject obj = case obj of
  OStr str -> return . text $ "\"" ++ T.unpack str ++ "\""
  ORegex _str -> undefined -- return $ "/" ++ T.unpack str ++ "/"
  OShellCommand str -> return . text $ "$ " ++ T.unpack str
  ODouble val -> return . text $ show val
  OBool val -> return . text $ show val
  ONum val -> return . text $ show val
  OVector vec -> do
    vec' <- V.mapM showObject vec
    if V.null vec' then
      return $ text "[]"
    else
      return $ V.foldl
        (\acc v -> acc <> ", " <> v)
        (text "[ " <> vec' V.! 0)
        (V.drop 1 vec')
        <> " ]"
  ORef rfs -> do
    obj' <- follow rfs
    showObject obj' <&> (text "*" <>) -- TODO: Remove when the project turn it more stable
  ONone -> return "None"
  OFunc _env args body -> do
    body' <- pprint (body (repeat ONone))
    return $ text "Function with args: " <> text (show args) <> text "{" $$
      nest 2 body' $$
      text "}"
  ONative _ -> return . text $ show obj
  OClassDef name _ _ -> return $ text "Class " <> text (T.unpack name)
  OObject _ methods -> do
    dic <- mapM (\(key, obj') -> do
      objDoc <- showObject (ORef obj')
      return $ text (T.unpack key) <> text " -> " <> objDoc) $ M.toList methods
    return $ text "{" $$
      nest 2 (vcat dic)$$
      text "}"

-- | Execute a sequence of instructions
runProgram :: FreeT Instruction StWorld Object -> StWorld Object
runProgram = iterT $ \case
  -- Find into world function and correspondent objects
  CallCommand _ idFun args next -> do
    retObj  <- callObject idFun args
    next retObj

  Assign _ idObj object next -> do
    -- object <- getObject accObject
    addObject idObj object
    next object -- TODO: debe retornar un ORef

  DropVar _ idObj next -> do
    dropVarWorld idObj
    next

  Loop _ accObject prog next -> do
    -- oIter <- getObject accObject
    _     <- mapObj accObject (runProgram . prog)
    next

  Cond _ objectCond trueNext falseNext next -> do
    bool <-  checkBool objectCond
    if bool
      then runProgram trueNext >>= next
      else runProgram falseNext >>= next

  GetVal _ idObj next -> do
    ref' <- findObject idObj
    next ref'

typeName :: Object -> String
typeName obj = case obj of
  OClassDef{}     -> "ClassDef"
  ONative{}       -> "Native"
  OFunc{}         -> "Function"
  OStr{}          -> "Str"
  ORegex{}        -> "Regex"
  OShellCommand{} -> "ShellCommand"
  ODouble{}       -> "Double"
  OBool{}         -> "Bool"
  ONum{}          -> "Num"
  OVector{}       -> "Vector"
  ORef{}          -> "Ref"
  ONone           -> "None"
  OObject{}       -> "Object"
