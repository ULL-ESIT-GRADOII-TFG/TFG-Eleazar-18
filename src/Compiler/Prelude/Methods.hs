{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.Methods where

import           Control.Monad.Except
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
-- import           System.Directory TODO

import           Compiler.Error
import           Compiler.Instruction
import           Compiler.Interpreter
import           Compiler.Object
import           Compiler.Types
import           Compiler.World


-- | Prelude load action
-- TODO: Create a class to interact with http connections
loadPrelude :: Interpreter ()
loadPrelude =
  -- Basic functions available on start
  liftWorld $ mapM_ (\(name, obj) -> newVarWithName name obj) baseBasicFunctions

-- | Build a method for metaclass (Specific use)
-- TODO: Revise error paths
internalMethod :: T.Text -> (T.Text, Object StWorld)
internalMethod name =
  ( name
  , ONative $ \case
    []             -> throw $ NumArgsMissmatch 0 1
    objs@(obj : _) ->
      case getMethods obj name of
        Just addr -> lift $ call (simple addr) objs
        Nothing   -> undefined --  TODO: lift $ liftScope $ throw $ NotDefinedObject name
  )

-- |
-- TODO: Add specific functions to modify internal interpreter variables. Like prompt, or path options ...
baseBasicFunctions :: [(T.Text, Object StWorld)]
baseBasicFunctions = []
{-   [ ("print"      , ONative (normalize printObj))
  , ("not"        , ONative (normalizePure not))
  , ("cd"         , ONative (normalize setCurrentDirectory))
  , ("use"        , ONative (normalize changeObject))
  , ("unuse"      , ONative (normalize changeObject))
  , ("dir"        , ONative (normalize changeObject))
  , ("docs"       , ONative (normalize changeObject))
  , ("self"       , ONative (normalize changeObject))
  , ("save_config", ONative (normalize changeObject))
  , ("load_config", ONative (normalize changeObject)) -- __config__
  ]
  ++ map internalMethod (M.keys operatorsPrecedence) -}

printObj :: Object StWorld -> ProgInstr StWorld
printObj obj = case obj of
  OObject (Just classRef) _attrs -> do
    clsObj <- lift $ unwrap . fst <$> findPathVar (simple classRef)
    case clsObj of
      OClassDef name _ref methods -> case M.lookup "__print__" methods of
        Just func' -> do
          func'' <- lift $ follow func'
          obj'   <- lift $ directCall func'' [obj]
          docs   <- lift $ showObject obj'
          liftIO $ putDoc docs
          return ONone
        Nothing -> do
          docs <- lift $ (pretty name <+>) <$> showObject obj
          liftIO $ putDoc docs
          return ONone
      _ -> do
        docs   <- lift $ showObject obj
        liftIO $ putDoc docs
        return ONone
  _ -> do
    docs   <- lift $ showObject obj
    liftIO $ putDoc docs
    return ONone

-- ROADMAP:
-- - get object __implicit__ to generate matched function with self passed
--
changeObject :: Object StWorld -> ProgInstr StWorld
changeObject = undefined

self :: ProgInstr StWorld
self = undefined
