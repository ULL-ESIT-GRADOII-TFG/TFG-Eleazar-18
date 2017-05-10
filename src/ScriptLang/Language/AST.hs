{-# LANGUAGE DeriveGeneric #-}
module ScriptLang.Language.AST where

import           Data.Set         (Set)
import           Data.Text        (Text)
import           Data.Vector      (Vector)

import           GHC.Generics

import           ScriptLang.Types

-- | Program is a sequence of statements with capacity to store information
type Program a = [Statement a]

type NameId = Text

-- | New tokens to specific interpreter use.
data REPL a
  = Exit
  | Help
  | MultilineOpen
  | MultilineClose
  | Code (Program a)
  deriving (Generic, Show, Eq)

-- | Statements of language
data Statement a
  = SType a [(Maybe Restrictions, NameId, Program ())]
  -- ^ Build a object into language
  | SFunction a NameId [NameId] (Program a)
  -- ^ Function with parameters and program
  | SMkScope a Exp (Program a)
  -- ^ Defines a new scope into code, it make implicit access to Exp into
  -- scope Program
  | SFor a Exp (Maybe NameId) (Program a)
  -- ^ A for loop with bind expresion to named variable, and contents of for loop
  | SWhile a Exp (Program a)
  -- ^ While loop
  | SIf a Exp (Program a)
  -- ^ if ... then ... conditional
  | SIfElse a Exp (Program a) (Program a)
  -- ^ if ... then ... else ... conditional
  | SExpr a Exp
  -- ^ A expresion
  deriving (Generic, Show, Eq)

-- | Expressions of language
data Exp
  = EApply [NameId] [Exp]
  -- ^ Apply it is call function or operator, itself include assign operator.
  -- `[NameId]` path to find object with dot notation Example
  -- > num.negate + 2
  -- [SExpr () (EApply ['num', 'negate'] []), SExpr () (EApply ["+"] [EInt 2])])
  | EShell Text
  -- ^ Represents a shell command to be executed
  | EString Text
  -- ^ A literal string
  | EInt Int
  -- ^ A simple int
  | EDouble Double
  -- ^ A simple Double
  | ERegex Text
  -- ^ Represent a regex, using a PCRE library
  | EBool Bool
  -- ^ A simple bool
  | ESet [Exp]
  -- ^ A Simple set data type
  | EVector [Exp]
  -- ^ A vector
  | EDict [(Exp, Exp)]
  -- ^ A dictionary like python o haskell Map
  | ENone
  -- ^ A nil, none, null expresion
  deriving (Generic, Show, Eq)
