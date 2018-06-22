{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.Methods where

import           Control.Monad.Except
import           Control.Monad.Trans.Free
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           System.Directory

import           Compiler.Error
import           Compiler.Interpreter.Utils
import           Compiler.Prelude.Types
import           Compiler.Prelude.Utils
import           Compiler.Types
import           Compiler.World.Methods


-- | Dictionary of operators precedence order
operatorsPrecedence :: M.Map T.Text (Int, Assoc)
operatorsPrecedence = M.fromList
  [ ("**", (8, LeftAssoc))
  , ("*" , (7, LeftAssoc))
  , ("/" , (7, LeftAssoc))
  , ("%" , (7, LeftAssoc))
  , ("+" , (6, LeftAssoc))
  , ("-" , (6, LeftAssoc))
  , ("++", (5, RightAssoc))
  , ("==", (4, LeftAssoc))
  , ("!=", (4, LeftAssoc))
  , ("/=", (4, LeftAssoc))
  , (">" , (4, LeftAssoc))
  , ("<" , (4, LeftAssoc))
  , ("<=", (4, LeftAssoc))
  , (">=", (4, LeftAssoc))
  , ("&&", (3, RightAssoc))
  , ("||", (3, RightAssoc))
  , ("??", (1, RightAssoc))
  , ("!" , (1, LeftAssoc))
  ]

-- | Prelude load action
-- TODO: Create a class to interact with http connections
loadPrelude :: Interpreter ()
loadPrelude =
  -- Basic functions available on start
  mapM_ (uncurry newVar) baseBasicFunctions

-- | Build a method for metaclass (Specific use)
-- TODO: Revise error paths
internalMethod :: T.Text -> (T.Text, Object)
internalMethod name =
  ( name
  , ONative $ \case
    []           -> throw $ NumArgsMissmatch 0 1
    objs@(obj:_) -> case getMethods obj name of
      Just func -> func objs
      Nothing   -> lift $ liftScope . throw $ NotDefinedObject name
  )

-- |
-- TODO: Add specific functions to modify internal interpreter variables. Like prompt, or path options ...
baseBasicFunctions :: [(T.Text, Object)]
baseBasicFunctions =
  [ ("print", ONative (normalize printObj))
  , ("not"  , ONative (normalizePure not))
  , ("cd"   , ONative (normalize setCurrentDirectory))
  , ("co"   , ONative (normalize changeObject))
  ]
  ++ map internalMethod (M.keys operatorsPrecedence)

-- TODO: use __print__
printObj :: Object -> FreeT Instruction StWorld Object
printObj obj = do
  liftIO $ print obj
  return ONone

changeObject :: Object -> FreeT Instruction StWorld Object
changeObject = undefined
