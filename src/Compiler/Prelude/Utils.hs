{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Compiler.Prelude.Utils where

import qualified Data.Map                 as M
import qualified Data.Text                as T

-- import           Compiler.Object

data Assoc = LeftAssoc | RightAssoc deriving (Show, Eq)

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
  -- , ("??", (1, RightAssoc, "__err__")) -- I keep to manage errors
  , ("!" , (1, LeftAssoc, "__not__"))
  , ("@" , (1, LeftAssoc, "__at__"))
  ]

-------------------------------------------------------------------------------
-- * Th generate functions
-- normalizePure
--   :: (ToObject o mm, FromObject a mm, MemoryManagement mm, RawObj mm ~ Object mm)
--   => (a -> o)
--   -> [Object mm]
--   -> Prog mm mm (RawObj mm)
-- normalizePure fun = normalize (toObject . fun)

-- normalizePure'
--   :: (ToObject o mm, FromObject a mm, FromObject b mm, MemoryManagement mm)
--   => (a -> b -> o)
--   -> [Object mm]
--   -> Prog mm mm (RawObj mm)
-- normalizePure' = $(normalizeArity 2)

-- normalizePure''
--   :: (ToObject o mm, FromObject a mm, FromObject b mm, FromObject c mm, MemoryManagement mm)
--   => (a -> b -> c -> o)
--   -> [Object mm]
--   -> Prog mm mm (RawObj mm)
-- normalizePure'' = $(normalizeArity 3)
