{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}
module Compiler.Prelude where

import           Control.Monad.Except
import qualified Data.HashMap.Strict                   as HM
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Vector                           as V
import           Lens.Micro.Platform
import           System.Console.Haskeline
import           System.Directory

import           Compiler.Config
import           Compiler.Error
import           Compiler.Interpreter
import           Compiler.Object
import           Compiler.Prelude.Github
import           Compiler.Prelude.Th
import           Compiler.Types


-- | Prelude load action
loadPrelude :: Interpreter ()
loadPrelude = do
  -- Basic functions available on start
  liftWorld $ mapM_ (uncurry newVarWithName) baseBasicFunctions
  void githubClassSC

-- | Build a method for metaclass (Specific use)
internalMethod :: T.Text -> (T.Text, Object)
internalMethod name =
  ( name
  , ONative $ \case
      []             -> throw $ NumArgsMissmatch 0 1 -- It shouldnt happen
      addresses@(addr : _) -> do
        obj <- unwrap <$> getVar addr
        addr' <- access obj name
        call (simple addr') addresses
  )


-- | A set of basic functions avaialable in the base scope
baseBasicFunctions :: [(T.Text, Object)]
baseBasicFunctions =
  [ ("print", ONative printWrapper)
  , ("cd"   , createONative $(normalize [| setCurrentDirectory :: String -> IO () |]))
  , ("not"  , createONative $(normalize [| not :: Bool -> Bool |]))
  , ("get_line", createONative $(normalize [| getLineWithAsk :: String -> IO (Maybe String) |]))
  , ("ask_password", createONative $(normalize [| askPassword :: Maybe Char -> String -> IO (Maybe String) |]))
  , ("__call__", ONative internalCall)
  , ("save_config", createONative $(normalize [| saveSubConfig :: String -> Object -> StWorld () |]))
  , ("load_config", createONative $(normalize [| loadSubConfig :: String -> StWorld Object |]))
  , ("use"        , createONative $(normalize [| layerObjectIntoScope :: Object -> StWorld Object |]))
  , ("unuse"      , createONative $(normalize [| unlayerObjectIntoScope :: StWorld () |]))
  , ("dir"        , createONative $(normalize [| (fmap (fmap fst) . exploreObjectAttributes) :: Object -> StWorld (V.Vector T.Text)|]))
  , ("==", ONative equivalence)
  , ("!=", ONative nonequivalence)
  , ("/=", ONative nonequivalence)
  ]
  ++ map internalMethod
    ["**",
    "*" ,
    "/" ,
    "%" ,
    "+" ,
    "-" ,
    "++",
    ">" ,
    "<" ,
    "<=",
    ">=",
    "&&",
    "||",
    "!" ,
    "@"]

equivalence :: [Address] -> StWorld Address
equivalence [addr1, addr2] = do
  ob1 <- unwrap <$> getVar addr1
  ob2 <- unwrap <$> getVar addr2
  if typeEquivalence ob1 ob2 then do
    addr <- access ob1 "=="
    call (simple addr) [addr1, addr2]
  else
    newVar . wrap $ OBool False
equivalence addrs = throw $ NumArgsMissmatch (length addrs) 2 -- It shouldnt happend


nonequivalence :: [Address] -> StWorld Address
nonequivalence [addr1, addr2] = do
  ob1 <- unwrap <$> getVar addr1
  ob2 <- unwrap <$> getVar addr2
  if typeEquivalence ob1 ob2 then do
    addr <- access ob1 "!="
    call (simple addr) [addr1, addr2]
  else
    newVar . wrap $ OBool True
nonequivalence addrs = throw $ NumArgsMissmatch (length addrs) 2 -- It shouldnt happend

createONative :: ([Object] -> StWorld Object) -> Object
createONative func = ONative $ \addrs -> do
  objs <- mapM (fmap unwrap . getVar) addrs
  obj <- func objs
  newVar $ wrap obj

internalCall :: [Address] -> StWorld Address
internalCall [] = throw $ WorldError "Report Error: Internal Call without args"
internalCall (arg:args) = call (simple arg) args

printWrapper :: [Address] -> StWorld Address
printWrapper [addr] = do
  printObj addr
  newVar $ wrap ONone
printWrapper ls = throw $ NumArgsMissmatch 1 (length ls)

printObj :: Address -> StWorld ()
printObj addr = unwrap <$> getVar addr >>= \case
  obj@(OObject (Just classRef) _attrs) -> do
    clsObj <- unwrap . fst <$> findPathVar (simple classRef)
    case clsObj of
      OClassDef name methods -> case HM.lookup "__print__" methods of
        Just func' -> do
          retAddr   <- call (PathVar func' []) [addr]
          obj' <- unwrap <$> getVar retAddr
          docs   <- showObject obj'
          liftIO $ putDoc docs
          liftIO $ putStr "\n"
        Nothing -> do
          docs <- (pretty name <+>) <$> showObject obj
          liftIO $ putDoc docs
          liftIO $ putStr "\n"
      _ -> do
        docs   <- showObject obj
        liftIO $ putDoc docs
        liftIO $ putStr "\n"
  obj -> do
    docs   <- showObject obj
    liftIO $ putDoc docs
    liftIO $ putStr "\n"


getLineWithAsk :: String -> IO (Maybe String)
getLineWithAsk ask = runInputT defaultSettings $ getInputLine ask

askPassword :: Maybe Char -> String -> IO (Maybe String)
askPassword mChar ask = runInputT defaultSettings $ getPassword mChar ask

typeEquivalence :: Object -> Object -> Bool
typeEquivalence obj1 obj2 = case (obj1, obj2) of
  (OClassDef{}, OClassDef{})         -> True
  (ONative{}, ONative{})             -> True
  (OFunc{}, OFunc{})                 -> True
  (OStr{}, OStr{})                   -> True
  (ORegex{}, ORegex{})               -> True
  (OBound{}, OBound{})               -> True
  (OShellCommand{}, OShellCommand{}) -> True
  (ODouble{}, ODouble{})             -> True
  (OBool{}, OBool{})                 -> True
  (ONum{}, ONum{})                   -> True
  (OVector{}, OVector{})             -> True
  (ORef{}, ORef{})                   -> True
  (ONone, ONone)                     -> True
  (OObject{}, OObject{})             -> True
  (ONativeObject{}, ONativeObject{}) -> True
  _                                  -> False

-- | Explore all accessors from a given object
exploreObjectAttributes :: Object -> StWorld [(T.Text, Address)]
exploreObjectAttributes obj = case obj of
  OObject mClassId properties -> do
    let objProperties = HM.toList properties
    case mClassId of
      Just ref -> do
        classObj <- unwrap <$> getVar ref
        classProperties <- exploreObjectAttributes classObj
        return $ objProperties ++ classProperties
      Nothing -> return objProperties
  ORef ref -> do
    obj' <- unwrap <$> getVar ref
    exploreObjectAttributes obj'
  OClassDef _name attrs -> return $ HM.toList attrs
  _ -> do
    methods <- internalMethods obj
    return $ HM.toList methods

-- | Adds a layer into scope
layerObjectIntoScope :: Object -> StWorld Object
layerObjectIntoScope obj@(ORef address) = do
  dic <- exploreObjectAttributes obj
  lAddress <- mapM (\(name, _addr) -> do
                       addr' <- snd <$> (mkRef (PathVar address [name]) :: StWorld (Object, Address))
                       return (name, PathVar addr' [])
                   ) dic
  let objScope = ScopeInfo (HM.fromList lAddress)
  scopeA.stackScopeA %= (++ [objScope])
  return obj
layerObjectIntoScope _ = undefined

-- | Remove a module layer from scope
unlayerObjectIntoScope :: StWorld ()
unlayerObjectIntoScope = do
  stackScope <- use $ scopeA.stackScopeA
  if length stackScope >  0 then do
    mapM_ (\pathVar -> deleteVar (pathVar^.refA)) (last stackScope ^. renameInfoA & HM.elems)
    scopeA.stackScopeA .= take (length stackScope - 1) stackScope
  else do
    liftIO $ putStrLn "No layered object to be unlayered"
    return ()
