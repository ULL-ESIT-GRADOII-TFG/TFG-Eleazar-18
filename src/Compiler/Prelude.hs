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
import           System.Console.Haskeline
import           System.Directory

import           Compiler.Config
import           Compiler.Error
import           Compiler.Interpreter
import           Compiler.Object                       ()
import           Compiler.Prelude.Github
import           Compiler.Prelude.Th
import           Compiler.Prelude.Utils
import           Compiler.Types


-- | Prelude load action
-- TODO: Create a class to interact with http connections
loadPrelude :: Interpreter ()
loadPrelude = do
  -- Basic functions available on start
  liftWorld $ mapM_ (uncurry newVarWithName) baseBasicFunctions
  void githubClassSC

-- | Build a method for metaclass (Specific use)
-- TODO: Revise error paths
internalMethod :: T.Text -> (T.Text, Object)
internalMethod name =
  ( name
  , ONative $ \case
    []             -> throw $ NumArgsMissmatch 0 1 -- It shouldnt happend
    objs@(obj : _) -> do
      addr <- access obj name
      call (simple addr) objs
  )

-- |
-- TODO: Add specific functions to modify internal interpreter variables. Like prompt, or path options ...
baseBasicFunctions :: [(T.Text, Object)]
baseBasicFunctions =
  [ ("print", ONative $(normalize [| printObj :: Object -> StWorld () |]))
  , ("cd"   , ONative $(normalize [| setCurrentDirectory :: String -> IO () |]))
  , ("not"  , ONative $(normalize [| not :: Bool -> Bool |]))
  , ("get_line", ONative $(normalize [| getLineWithAsk :: String -> IO (Maybe String) |]))
  , ("ask_password", ONative $(normalize [| askPassword :: Maybe Char -> String -> IO (Maybe String) |]))
  , ("__call__", ONative internalCall)
  , ("save_config", ONative $(normalize [| saveSubConfig :: String -> StWorld Object |]))
  , ("load_config", ONative $(normalize [| loadSubConfig :: String -> StWorld Object |])) -- __config__
  , ("use"        , ONative $(normalize [| layerObjectIntoScope :: Object -> StWorld Object |]))
  , ("unuse"      , ONative $(normalize [| unlayerObjectIntoScope :: StWorld () |]))
  , ("dir"        , ONative $(normalize [| (fmap (fmap fst) . exploreObjectAttributes) :: Object -> StWorld (V.Vector T.Text)|]))
  -- , ("docs"       , ONative (normalize TODO))
  -- , ("self"       , ONative (normalize TODO))
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

-- TODO
exploreObjectAttributes :: Object -> StWorld (V.Vector (T.Text, Address))
exploreObjectAttributes = undefined

-- TODO:
layerObjectIntoScope :: Object -> StWorld Object
layerObjectIntoScope obj = undefined
  -- -- It must generate OBound objects
  -- set <- getObjectIdentifiers obj :: StWorld [(T.Text, Object)]
  -- newVarWithName
  -- addTopScope def

-- TODO
unlayerObjectIntoScope :: StWorld ()
unlayerObjectIntoScope = undefined
  -- if scopeNumber > 1 then
  --   addrs <- getAllObjects

  -- else
