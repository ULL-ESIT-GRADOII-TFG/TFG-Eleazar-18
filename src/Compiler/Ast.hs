module Compiler.Ast where

import           Compiler.Parser.Types
import qualified Data.Text             as T


data Repl
  = Command T.Text [T.Text]
  | Code [Statement TokenInfo]
  deriving Show

type Statement a = StatementG a T.Text

data StatementG a id
  = Import T.Text a
  | Class id (ExpressionG (AccessorG a) a id) (ExpressionG (AccessorG a) a id) a
  | Expr (ExpressionG (AccessorG a) a id) a
  deriving Show

type Expression a = ExpressionG (AccessorG a) a T.Text

-- | Generic representation of expression
data ExpressionG acc a id
  = FunDecl [id] (ExpressionG acc a id) a
  | VarDecl (acc id) (ExpressionG acc a id) a
  | SeqExpr [ExpressionG acc a id] a
  | If (ExpressionG acc a id) (ExpressionG acc a id) a
  | IfElse (ExpressionG acc a id) (ExpressionG acc a id) (ExpressionG acc a id) a
  | For id (ExpressionG acc a id) (ExpressionG acc a id) a
  | MkScope [ExpressionG acc a id]
  -- ^ Explicit scope
  | Apply (acc id) [ExpressionG acc a id] a
  | Identifier (acc id) a
  | Factor (AtomG acc a id) a
  deriving Show

type Accessor a = AccessorG a T.Text

data AccessorG a id
  = Dot id (AccessorG a id) a
  | Simple id a
  deriving Show

type Atom a = AtomG (AccessorG a) a T.Text

data AtomG acc a id
  = ANum Int
  | ADecimal Double
  | ARegex T.Text
  | AShellCommand T.Text
  | AStr T.Text
  | ABool Bool
  | AVector [ExpressionG acc a id]
  | ADic [(T.Text, ExpressionG acc a id)]
  | ANone
  deriving Show
