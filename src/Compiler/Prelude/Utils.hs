{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Prelude.Utils where

import           Control.Monad.Trans.Free
-- import qualified Data.IntMap                as IM
import qualified Data.Map                 as M
import qualified Data.Text                as T

-- import           Compiler.Interpreter.Utils
import           Compiler.Object.Types
import           Compiler.Prelude.Th
import           Compiler.Prelude.Types
-- import           Compiler.Scope.Utils
import           Compiler.Types
-- import           Compiler.World.Methods


-- | Generate a new class, with name and methods/properties given
-- newClass :: T.Text -> [(T.Text, Object)] -> Interpreter Word
-- newClass name attributes = do
--   classId <- liftWorld $ liftScope getNewId
--   let classDef = ClassDefinition name (M.fromList attributes)
--   memory . scope . typeDefinitions %= IM.insert (fromIntegral classId) classDef
--   return classId

-- -- | Create a object instance from referenced class
-- instanceClass :: Word -> T.Text -> Interpreter AddressRef
-- instanceClass classId name = newVar name (OObject (Just classId) mempty)

-- | Dictionary of operators precedence order
operatorsPrecedence :: M.Map T.Text (Int, Assoc, T.Text)
operatorsPrecedence = M.fromList
  [ ("**", (8, LeftAssoc, "__pow__"))
  , ("*" , (7, LeftAssoc, "__mul__"))
  , ("/" , (7, LeftAssoc, "__div__"))
  , ("%" , (7, LeftAssoc, "__mod__"))
  , ("+" , (6, LeftAssoc, "__plus__"))
  , ("-" , (6, LeftAssoc, "__minus__"))
  , ("++", (5, RightAssoc, "__append__"))
  , ("==", (4, LeftAssoc, "__eq__"))
  , ("!=", (4, LeftAssoc, "__neq__"))
  , ("/=", (4, LeftAssoc, "__neq__"))
  , (">" , (4, LeftAssoc, "__gt__"))
  , ("<" , (4, LeftAssoc, "__lt__"))
  , ("<=", (4, LeftAssoc, "__le__"))
  , (">=", (4, LeftAssoc, "__ge__"))
  , ("&&", (3, RightAssoc, "__and__"))
  , ("||", (3, RightAssoc, "__or__"))
  , ("??", (1, RightAssoc, "__err__")) -- I keep to manage errors
  , ("!" , (1, LeftAssoc, "__not__"))
  ]

-------------------------------------------------------------------------------
-- * Th generate functions
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
