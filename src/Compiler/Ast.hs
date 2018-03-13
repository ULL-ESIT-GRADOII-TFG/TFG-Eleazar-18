module Compiler.Ast where


import qualified Data.Text           as T


data Repl
  = Command T.Text [T.Text]
  | Code (Expression ())
  deriving Show

data Expression a
  = FunDecl [T.Text] (Expression a) a
  | VarDecl T.Text (Expression a) a
  | SeqExpr [Expression a] a
  | If (Expression a) (Expression a) a
  | IfElse (Expression a) (Expression a) (Expression a) a
  | For T.Text (Expression a) (Expression a) a
  | Apply T.Text [Expression a] a
  | Identifier T.Text a
  | Factor Atom a
  deriving Show

data Atom
  = ANum Int
  | AStr T.Text
  deriving Show

