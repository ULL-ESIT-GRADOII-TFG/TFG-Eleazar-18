{-# LANGUAGE TemplateHaskell #-}
module Compiler.Prelude.Utils where

import           Control.Monad.Trans.Free
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import           Data.Maybe
import qualified Data.Text                  as T

import           Compiler.Interpreter.Utils
import           Compiler.Object.Types
import           Compiler.Prelude.Th
import           Compiler.Prelude.Types
import           Compiler.Scope.Utils
import           Compiler.Types


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
