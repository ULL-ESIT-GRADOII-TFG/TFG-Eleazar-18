{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Compiler.Prelude.Methods where

import           Control.Monad.Trans.Free
import           Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Map as M

import Compiler.Prelude.Types
import Compiler.Prelude.Th
import Compiler.Object.Types
import Compiler.World.Types
import Compiler.Instruction.Types


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

baseObjects :: [(T.Text, Object)]
baseObjects =
    [ ("print", ONative (normalize printObj))
    , ("not"  , ONative (normalizePure not))
    , ("sum"  , ONative (normalizePure' ((+) :: Int -> Int -> Int)))
    ]

printObj :: Object -> FreeT Instruction StWorld Object
printObj obj = liftIO $ print obj >> return ONone

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
