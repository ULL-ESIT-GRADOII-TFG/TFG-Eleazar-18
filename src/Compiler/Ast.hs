module Compiler.Ast where

import           Compiler.Parser.Types
import qualified Data.Text             as T


data Repl
  = Command T.Text [T.Text]
  | Code [Statement TokenInfo]
  deriving Show

data Statement a
  = Import T.Text a
  | ClassSt (ClassDecl a)
  | FunSt (FunDecl a)
  | Expr (Expression a)
  deriving Show

data ClassDecl a = ClassDecl T.Text [Expression a] [FunDecl a] a deriving Show

data FunDecl a = FunDecl T.Text [T.Text] (Expression a) a deriving Show

-- | Generic representation of expression
data Expression a
  = FunExpr [T.Text] (Expression a) a
  | VarExpr (Accessor a) (Expression a) a
  | SeqExpr [Expression a] a
  | If (Expression a) (Expression a) a
  | IfElse (Expression a) (Expression a) (Expression a) a
  | For T.Text (Expression a) (Expression a) a
  | MkScope [Expression a]
  -- ^ Explicit scope
  | Apply (Accessor a) [Expression a] a
  | Identifier (Accessor a) a
  | Factor (Atom a) a
  deriving Show

data Accessor a
  = Dot T.Text (Accessor a) a
  | Simple T.Text a
  deriving Show

data Atom a
  = ANum Int
  | ADecimal Double
  | ARegex T.Text
  | AShellCommand T.Text
  | AStr T.Text
  | ABool Bool
  | AVector [Expression a]
  | ADic [(T.Text, Expression a)]
  | ANone
  deriving Show
