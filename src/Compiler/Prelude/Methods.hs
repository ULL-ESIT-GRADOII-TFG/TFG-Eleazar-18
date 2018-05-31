{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.Methods where

import           Control.Monad.Except
import           Control.Monad.Trans.Free
import qualified Data.IntMap                    as IM
import qualified Data.Map                       as M
import qualified Data.Text                      as T

import           Compiler.Interpreter.Utils
import           Compiler.Object.Methods
import qualified Compiler.Prelude.OBool         as OBool
import qualified Compiler.Prelude.ODic          as ODic
import qualified Compiler.Prelude.ODouble       as ODouble
import qualified Compiler.Prelude.ONum          as ONum
import qualified Compiler.Prelude.ORegex        as ORegex
import qualified Compiler.Prelude.OShellCommand as OShellCommand
import qualified Compiler.Prelude.OStr          as OStr
import qualified Compiler.Prelude.OVector       as OVector
import           Compiler.Prelude.Types
import           Compiler.Prelude.Utils
import           Compiler.Types


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
  ]

-- | Prelude load action
-- TODO: Create a class to interact with http connections
loadPrelude :: Interpreter ()
loadPrelude = do
  -- Special Constructor for objects
  memory.table %= IM.insert 0 (def { _rawObj = ONative instanceObject})
  -- Basic functions available on start
  mapM_ (uncurry newVar) baseBasicFunctions

-- | Build a method for metaclass (Specific use)
-- TODO: Revise error paths
internalMethod :: T.Text -> (T.Text, Object)
internalMethod name =
  ( name
  , ONative $ \case
    []           -> throwError NumArgsMissmatch
    objs@(obj:_) -> case getMethods obj name of
      Just func -> func objs
      Nothing   -> throwError NotFoundObject
  )

-- TODO Reorganize to add internal docs
getMethods :: Object -> T.Text -> Maybe ([Object] -> Prog)
getMethods obj name = case obj of
  OStr _                   -> OStr.methods name
  OBool _val               -> OBool.methods name
  ODouble _val             -> ODouble.methods name
  ONum _val                -> ONum.methods name
  OVector _val             -> OVector.methods name
  ODic _val                -> ODic.methods name
  ORegex        _str       -> ORegex.methods name
  OShellCommand _str       -> OShellCommand.methods name
  OFunc _bind _args _prog  -> Nothing
  ONative _func            -> Nothing
  OObject _classId _dicObj -> Nothing
  ORef _rfs                -> Nothing
  ONone                    -> Nothing

-- |
-- TODO: Add specific functions to modify internal interpreter variables. Like prompt, or path options ...
baseBasicFunctions :: [(T.Text, Object)]
baseBasicFunctions =
  [ ("print", ONative (normalize printObj))
  , ("not"  , ONative (normalizePure not))
  ]
  ++ map internalMethod (M.keys operatorsPrecedence)

-- TODO: use __print__
printObj :: Object -> FreeT Instruction StWorld Object
printObj obj = do
  liftIO $ print obj
  return ONone

instanceObject :: [Object] -> FreeT Instruction StWorld Object
instanceObject objs = case objs of
  (ONum idRef : args) -> do
    defs <- use $ scope.typeDefinitions
    case IM.lookup idRef defs of
      Just clsDef -> do
        let self = OObject (Just $ fromIntegral idRef) mempty
        case M.lookup "__init__" (clsDef^.attributesClass) of
          Just method -> lift $ callObjectDirect method args >> return self
          Nothing -> if not $ null objs then lift $ throwError NumArgsMissmatch else return self
      Nothing -> lift $ throwError NotFoundObject
  _ -> lift $ throwError $ WorldError "instanceObject: wrong parameters"
