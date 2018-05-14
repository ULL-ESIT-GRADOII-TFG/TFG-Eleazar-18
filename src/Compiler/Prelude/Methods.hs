{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Prelude.Methods where

import           Control.Monad.Except
import           Control.Monad.Trans.Free
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Vector                as V
import           Lens.Micro.Platform
import           System.Process

import           Compiler.Instruction.Types
import           Compiler.Interpreter.Utils
import           Compiler.Object.Types
import           Compiler.Prelude.Th
import           Compiler.Prelude.Types
import           Compiler.Scope.Methods
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
loadPrelude = mapM_ (uncurry newVar) baseBasicFunctions
  -- idClass <- newClass "MetaClass" $
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
    []           -> throwError NumArgsMissmatch
    objs@(obj:_) -> case getMethods obj name of
      Just func -> func objs
      Nothing   -> throwError NotFoundObject
  )

-- TODO Reorganize to add internal docs
getMethods :: Object -> T.Text -> Maybe ([Object] -> Prog)
getMethods obj name = case obj of
  OStr _str -> case name of
    "++"    -> Just $ normalizePure' (T.append)
    "strip" -> Just $ normalizePure (T.strip)
    _       -> Nothing
  OBool _val -> case name of
    "!"  -> Just $ normalizePure not
    "||" -> Just $ normalizePure' (||)
    "&&" -> Just $ normalizePure' (&&)
    _    -> Nothing
  -- TODO: Add negate operator
  ODouble _val -> case name of
    "*" -> Just $ normalizePure' ((*) :: Double -> Double -> Double)
    "/" -> Just $ normalizePure' ((/) :: Double -> Double -> Double)
    "+" -> Just $ normalizePure' ((+) :: Double -> Double -> Double)
    "-" -> Just $ normalizePure' ((-) :: Double -> Double -> Double)
    _   -> Nothing
  ONum _val -> case name of
    "*" -> Just $ normalizePure' ((*) :: Int -> Int -> Int)
    "/" -> Just $ normalizePure' (div :: Int -> Int -> Int)
    "+" -> Just $ normalizePure' ((+) :: Int -> Int -> Int)
    "-" -> Just $ normalizePure' ((-) :: Int -> Int -> Int)
    _   -> Nothing
  OVector _val -> case name of
    "len" -> Just $ normalizePure (V.length :: V.Vector Object -> Int)
    "++"  -> Just $ normalizePure'
      (mappend :: V.Vector Object -> V.Vector Object -> V.Vector Object)
    _ -> Nothing
  ODic _val -> case name of
    _ -> Nothing
  ORegex        _str -> Nothing
  OShellCommand _str -> case name of
    "exec" -> Just $ normalize execProcess
    -- "!" -> Just $ normalizePure' (mappend :: V.Vector Object -> V.Vector Object -> V.Vector Object)
    _      -> Nothing
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
    ++ map
         internalMethod
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
  listRefs      <- mapM ((_ref . fromJust <$>) . uncurry newVar) attributes
  let
    classDef =
      ClassDefinition name (M.fromList (zip (map fst attributes) listRefs))
  memory . scope . typeDefinitions %= IM.insert (fromIntegral classId) classDef
  return classId

-- | Create a object instance from referenced class
instanceClass :: Word -> T.Text -> Interpreter (Maybe AddressRef)
instanceClass classId name = newVar name (OObject (Just classId) mempty)

printObj :: Object -> FreeT Instruction StWorld Object
printObj obj = liftIO $ print obj >> return ONone

-- | Execute a command into the shell
execProcess :: Object -> FreeT Instruction StWorld Object
execProcess (OShellCommand text) = do
  value <- liftIO $ do
    (_, hout, _, h) <- createProcess ((shell $ T.unpack text) { std_out = CreatePipe })
    maybe (return "") T.hGetContents hout
  return $ OStr value


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
