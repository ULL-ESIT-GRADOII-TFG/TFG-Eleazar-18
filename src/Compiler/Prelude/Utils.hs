{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Compiler.Prelude.Utils where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T


data Assoc = LeftAssoc | RightAssoc deriving (Show, Eq)

-- | Dictionary of operators precedence order
operatorsPrecedence :: HM.HashMap T.Text (Int, Assoc, T.Text)
operatorsPrecedence = HM.fromList
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