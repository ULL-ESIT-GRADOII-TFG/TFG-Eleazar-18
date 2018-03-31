module Compiler.Ast where


import qualified Data.Text             as T

import           Compiler.Parser.Types

data Repl
  = Command T.Text [T.Text]
  | Code ([Statement TokenInfo])
  deriving Show

type Statement a = StatementG a T.Text

data StatementG a id
  = Import T.Text a
  | Class id (ExpressionG a id) a
  | Expr (ExpressionG a id) a
  deriving Show

type Expression a = ExpressionG a T.Text

-- | Generic representation of expression
data ExpressionG a id
  = FunDecl [id] (ExpressionG a id) a
  | VarDecl id (ExpressionG a id) a
  | SeqExpr [ExpressionG a id] a
  | If (ExpressionG a id) (ExpressionG a id) a
  | IfElse (ExpressionG a id) (ExpressionG a id) (ExpressionG a id) a
  | For id (ExpressionG a id) (ExpressionG a id) a
  | Apply id [ExpressionG a id] a
  | Identifier id a
  | Factor Atom a
  deriving Show

data Atom
  = ANum Int
  | ADecimal Double
  | ARegex T.Text
  | AShellCommand T.Text
  | AStr T.Text
  | ABool Bool
  deriving Show

-- | TODO: Tiene un gran problema tiene una alta dependencia del AST, y puede ser complicar el proceso
data CrumbExpression a
  = CFunArg a
  | CFun
  | CIf
  | CIfTrue
  | CIfFalse
  | CForIter
  | CForValue a
  | CForBody
  | CVar a
  -- | In this case indicate position of to access
  | CSeq Word
  -- |
  | CAppArg Word
  | CApp a
  | CId a

{-
Example:
  if test_1 {
    test_2
    test_3(arg_0, arg_1(val_0))
  }
  else {
    for obj_0 in iter_0 {
      test_4 = test_5
    }
  }

data Resolver a = Trie CrumbExpression a

[CIf "test_1"] -> test_1
[CIfTrue, CSeq 0, Id "test_2"] -> test_2
[CIfTrue, CSeq 1, CApp "test_3"] -> test_3
[CIfTrue, CSeq 1, CAppArg 0, Id "arg_0"] -> arg_0
[CIfTrue, CSeq 1, CAppArg 0, CApp "arg_1"] -> arg_1
[CIfTrue, CSeq 1, CAppArg 0, CAppArg 0, Id "val_0"] -> val_0
[CIfFalse, CForValue] ->
-}


