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
import           Compiler.Scope.Types
import           Compiler.Scope.Methods
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
-- TODO: Create a class to interact with http connections
loadPrelude :: Interpreter ()
loadPrelude =
  mapM_ (uncurry newVar) baseBasicFunctions
  -- idClass <- newClass "MetaClass" $
  --   -- TODO: Add basic operators, change operators name to method specific name. (Really neccesary do it?)
  --   map internalMethod
  --   [ "__brace__"
  --   , "__init__"
  --   , "__call__"
  --   ]
  -- void $ instanceClass idClass "self"

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

getMethods :: Object -> T.Text -> Maybe ([Object] -> Prog)
getMethods obj name = case obj of
  OStr _str               ->
    case name of
      "++" -> Just $ \_objs -> return ONone
      _    -> Nothing
  OBool _val              ->
    case name of
      "!"  -> Just $ normalizePure not
      "||" -> Just $ normalizePure' (||)
      "&&" -> Just $ normalizePure' (&&)
      _ -> Nothing
  -- TODO: Add negate operator
  ODouble _val            ->
    case name of
      "*" -> Just $ normalizePure' ((*) :: Double -> Double -> Double)
      "/" -> Just $ normalizePure' ((/) :: Double -> Double -> Double)
      "+" -> Just $ normalizePure' ((+) :: Double -> Double -> Double)
      "-" -> Just $ normalizePure' ((-) :: Double -> Double -> Double)
      _ -> Nothing
  ONum _val               ->
    case name of
      "*" -> Just $ normalizePure' ((*) :: Int -> Int -> Int)
      "/" -> Just $ normalizePure' (div :: Int -> Int -> Int)
      "+" -> Just $ normalizePure' ((+) :: Int -> Int -> Int)
      "-" -> Just $ normalizePure' ((-) :: Int -> Int -> Int)
      _ -> Nothing
  ORegex _str             -> Nothing
  OShellCommand _str      -> Nothing
  OFunc _bind _args _prog   -> Nothing
  ONative _func -> Nothing
  OObject _classId _dicObj -> Nothing
  ORef _rfs               -> Nothing
  ONone                  -> Nothing

-- |
-- TODO: Add specific functions to modify internal interpreter variables. Like prompt, or path options ...
baseBasicFunctions :: [(T.Text, Object)]
baseBasicFunctions =
  [ ("print", ONative (normalize printObj))
  , ("not"  , ONative (normalizePure not))
  ] ++
  map internalMethod
  [ "__brace__"
  , "__init__"
  , "__call__"
  , "print"
  , "**"
  , "*"
  , "/"
  , "%"
  , "+"
  , "-"
  , "++"
  , "=="
  , "!="
  , "/="
  , ">"
  , "<"
  , "<="
  , ">="
  , "&&"
  , "||"
  , "??"
  , "!"
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
  memory.scope.typeDefinitions %= IM.insert (fromIntegral classId) classDef
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
