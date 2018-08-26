{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Prelude.Methods where

import           Control.Monad.Except
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Directory

import           Compiler.Error
import           Compiler.Interpreter
import           Compiler.Object                       ()
import           Compiler.Prelude.Th
import           Compiler.Prelude.Utils
import           Compiler.Types


-- | Prelude load action
-- TODO: Create a class to interact with http connections
loadPrelude :: Interpreter ()
loadPrelude =
  -- Basic functions available on start
  liftWorld $ mapM_ (uncurry newVarWithName) baseBasicFunctions

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
  -- , ("use"        , ONative (normalize changeObject))
  -- , ("unuse"      , ONative (normalize changeObject))
  -- , ("dir"        , ONative (normalize changeObject))
  -- , ("docs"       , ONative (normalize changeObject))
  -- , ("self"       , ONative (normalize changeObject))
  -- , ("save_config", ONative (normalize changeObject))
  -- , ("load_config", ONative (normalize changeObject)) -- __config__
  ]
    ++ map internalMethod (M.keys operatorsPrecedence)

printObj :: Object -> StWorld ()
printObj obj = case obj of
  OObject (Just classRef) _attrs -> do
    clsObj <- unwrap . fst <$> findPathVar (simple classRef)
    case clsObj of
      OClassDef name _ref methods -> case M.lookup "__print__" methods of
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

-- ROADMAP:
-- - get object __implicit__ to generate matched function with self passed
--
-- changeObject :: Object -> ProgInstr Object
-- changeObject = undefined

-- self :: ProgInstr StWorld
-- self = undefined
