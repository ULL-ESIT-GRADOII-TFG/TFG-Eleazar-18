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
import           Compiler.Prelude.Utils
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
    []             -> throw $ NumArgsMissmatch 0 1 -- It shouldnt happend
    objs@(obj : _) -> do
      addr <- access obj name
      call (simple addr) objs
  )

-- | A set of basic functions avaialable in the base scope
baseBasicFunctions :: [(T.Text, Object)]
baseBasicFunctions =
  [ ("print", ONative $(normalize [| printObj :: Object -> StWorld () |]))
  , ("cd"   , ONative $(normalize [| setCurrentDirectory :: String -> IO () |]))
  , ("not"  , ONative $(normalize [| not :: Bool -> Bool |]))
  , ("get_line", ONative $(normalize [| getLineWithAsk :: String -> IO (Maybe String) |]))
  , ("ask_password", ONative $(normalize [| askPassword :: Maybe Char -> String -> IO (Maybe String) |]))
  , ("__call__", ONative internalCall)
  , ("save_config", ONative $(normalize [| saveSubConfig :: String -> Object -> StWorld () |]))
  , ("load_config", ONative $(normalize [| loadSubConfig :: String -> StWorld Object |]))
  , ("use"        , ONative $(normalize [| layerObjectIntoScope :: Object -> StWorld Object |]))
  , ("unuse"      , ONative $(normalize [| unlayerObjectIntoScope :: StWorld () |]))
  , ("dir"        , ONative $(normalize [| (fmap (fmap fst) . exploreObjectAttributes) :: Object -> StWorld (V.Vector T.Text)|]))
  ]
    ++ map internalMethod (HM.keys operatorsPrecedence)

internalCall :: [Object] -> StWorld Object
internalCall [] = throw $ WorldError "Report Error: Internal Call without args"
internalCall (ONum val:args) = call (simple val) args
internalCall _ = throw $ WorldError "Report Error: Internal Call doesn't recieve an int"

printObj :: Object -> StWorld ()
printObj obj = case obj of
  OObject (Just classRef) _attrs -> do
    clsObj <- unwrap . fst <$> findPathVar (simple classRef)
    case clsObj of
      OClassDef name _ref methods -> case HM.lookup "__print__" methods of
        Just func' -> do
          func'' <- follow func'
          obj'   <- directCall func'' [obj]
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
  _ -> do
    docs   <- showObject obj
    liftIO $ putDoc docs
    liftIO $ putStr "\n"


getLineWithAsk :: String -> IO (Maybe String)
getLineWithAsk ask = runInputT defaultSettings $ getInputLine ask

askPassword :: Maybe Char -> String -> IO (Maybe String)
askPassword mChar ask = runInputT defaultSettings $ getPassword mChar ask

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
  OClassDef _name _refClass attrs -> return $ HM.toList attrs
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
