{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.Methods where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Free
import qualified Data.IntMap                    as IM
import qualified Data.Map                       as M
import qualified Data.Text                      as T

import           Compiler.Interpreter.Utils
import           Compiler.Object.Methods
import qualified Compiler.Prelude.OBool         as OBool
import qualified Compiler.Prelude.ODouble       as ODouble
import qualified Compiler.Prelude.ONum          as ONum
import qualified Compiler.Prelude.ORegex        as ORegex
import qualified Compiler.Prelude.OShellCommand as OShellCommand
import qualified Compiler.Prelude.OStr          as OStr
import qualified Compiler.Prelude.OVector       as OVector
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
  ]

-- | Prelude load action
-- TODO: Create a class to interact with http connections
loadPrelude :: Interpreter ()
loadPrelude = do
  -- Special Constructor for objects
  memoryA.tableA %= IM.insert 0 (def { _rawObj = ONative instanceObject})
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
  OStr{}          -> OStr.methods name
  OBool{}         -> OBool.methods name
  ODouble{}       -> ODouble.methods name
  ONum{}          -> ONum.methods name
  OVector{}       -> OVector.methods name
  ORegex{}        -> ORegex.methods name
  OShellCommand{} -> OShellCommand.methods name
  OFunc{}         -> Nothing
  ONative{}       -> Nothing
  OObject{}       -> Nothing
  OClassDef{}     -> Nothing
  ORef{}          -> Nothing
  ONone           -> Nothing

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
    defs <- use tableA
    let classDef = IM.lookup idRef defs >>= \obj ->
          case obj^.rawObjA of
            OClassDef _ d -> return d
            _             -> Nothing
    case classDef of
      Just clsDef -> do
        self <- lift . newObject $ OObject (Just $ fromIntegral idRef) mempty
        case M.lookup "__init__" clsDef of
          Just method -> lift $ do
            _ <- callObjectDirect method (ORef self:args)
            return (ORef self)
          Nothing -> if null args then
                       return (ORef self)
                     else
                       lift $ throwError NumArgsMissmatch
      Nothing -> lift $ throwError NotFoundObject
  _ -> lift $ throwError $ WorldError "instanceObject: Wrong parameters"
