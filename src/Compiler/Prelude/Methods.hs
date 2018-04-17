{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Prelude.Methods where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Free
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import           Data.Maybe
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import           Compiler.Instruction.Types
import           Compiler.Interpreter.Types
import           Compiler.Interpreter.Utils
import           Compiler.Object.Types
import           Compiler.Prelude.Th
import           Compiler.Prelude.Types
import           Compiler.Scope.Methods
import           Compiler.World.Methods
import           Compiler.World.Types


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
loadPrelude :: Interpreter ()
loadPrelude = do
  mapM_ (uncurry newVar) baseBasicFunctions
  idClass <- newClass "MetaClass"
    -- TODO: Add basic operators, change operators name to method specific name. (Really neccesary do it?)
    [ internalMethod "__brace__"
    , internalMethod "__print__"
    , internalMethod "__call__"
    , internalMethod "__callable__"
  -- , ("+"  , ONative (normalizePure' ((+) :: Int -> Int -> Int)))
  -- , ("-"  , ONative (normalizePure' ((-) :: Int -> Int -> Int)))
  -- , ("/"  , ONative (normalizePure' (div :: Int -> Int -> Int)))
  -- , ("%"  , ONative (normalizePure' (mod :: Int -> Int -> Int)))
  -- , ("*"  , ONative (normalizePure' ((*) :: Int -> Int -> Int)))
  -- , ("**"  , ONative (normalizePure' ((**) :: Double -> Double -> Double)))
  -- , ("__brace__"  , ONative (normalizePure' ((**) :: Double -> Double -> Double)))
    ]
  void $ instanceClass idClass "self"

-- | Build a method for metaclass (Specific use)
-- TODO: Revise error paths
internalMethod :: T.Text -> (T.Text, Object)
internalMethod name =
  ( name
  , ONative $ \case
      []           -> return ONone --
      objs@(obj:_) ->
        case getMethods obj name of
          Just func -> func objs
          Nothing   -> return ONone) --

-- |
-- TODO: Add specific functions to modify internal interpreter variables. Like prompt, or path options ...
baseBasicFunctions :: [(T.Text, Object)]
baseBasicFunctions =
  [ ("print", ONative (normalize printObj))
  , ("not"  , ONative (normalizePure not))
  , ("!"  , ONative (normalizePure not)) -- TODO: Check unary operators in the parser
  , ("||"  , ONative (normalizePure' (||)))
  , ("&&"  , ONative (normalizePure' (&&)))
  ]

-- | Generate a new class, with name and methods/properties given
newClass :: T.Text -> [(T.Text, Object)] -> Interpreter Word
newClass name attributes = do
  Right classId <- liftScope getNewId
  listRefs <- mapM ((_ref . fromJust <$>) . uncurry newVar) attributes
  let
    classDef = ClassDefinition
      name
      (M.fromList (zip (map fst attributes) listRefs))
  memory.typeDefinitions %= IM.insert (fromIntegral classId) classDef
  return classId

-- | Create a object instance from referenced class
instanceClass :: Word -> T.Text -> Interpreter (Maybe AddressRef)
instanceClass classId name = newVar name (OObject (Just classId) mempty)

printObj :: Object -> FreeT Instruction StWorld Object
printObj obj = liftIO $ print obj >> return ONone


-------------------------------------------------------------------------------
-- * Th generate functions
-------------------------------------------------------------------------------
normalizePure
  :: (ToObject o, FromObject a)
  => (a -> o)
  -> [Object]
  -> FreeT Instruction StWorld Object
normalizePure fun = normalize (toObject . fun)

normalizePure'
  :: (ToObject o, FromObject a, FromObject b)
  => (a -> b -> o)
  -> [Object]
  -> FreeT Instruction StWorld Object
normalizePure' = $(normalizeArity 2)

normalizePure''
  :: (ToObject o, FromObject a, FromObject b, FromObject c)
  => (a -> b -> c -> o)
  -> [Object]
  -> FreeT Instruction StWorld Object
normalizePure'' = $(normalizeArity 3)
