module Compiler.Token.Types where
{-
  Improvements to this module, its depends from Lexer.x:
    - Move Show definition to a new function. Use the default Show.
-}

import           Data.Text


-- | Tokens available
data Token
  = OBraceT
  -- ^ "{"
  | CBraceT
  -- ^ "}"
  | OParenT
  -- ^ "("
  | CParenT
  -- ^ ")"
  | OBracketT
  -- ^ "["
  | CBracketT
  -- ^ "]"
  | ClassT
  -- ^ "class" keyword
  | ImportT
  -- ^ "import" keyword
  | LamT
  -- ^ "lam" keyword
  | FunT
  -- ^ "fun" keyword
  | ForT
  -- ^ "for" keyword
  | InT
  -- ^ "in" keyword
  | IfT
  -- ^ "if" keyword
  | ElseT
  -- ^ "else" keyword
  | CommaT
  -- ^ ","
  | AssignT
  -- ^ "="
  | ShellCommandT Text
  -- ^ "!$shell_command_name arg1 arg$" to execute a shell command
  | RegexExprT Text
  -- ^ "r"Project$digit${3,9}"
  | NameIdT Text
  -- ^ "hello"
  | ClassIdT Text
  -- ^ "Hello" Uppercase
  | LitTextT Text
  -- ^ A literal string
  | NumT Int
  -- ^ Represents a integer
  | DecimalT Double
  -- ^ Represents a decimal
  | BoolT Bool
  -- ^ "true" or "false"
  | NoneT
  -- ^ None action
  | OperatorT Text
  -- ^ Operators are symbols like +,-,*,/,++....
  | ICommandT Text [Text]
  -- ^ Command of interpreter
  | SkipT
  -- ^ Aux Token
  | DedentT Int
  -- ^ Dedent `x` levels
  | EndStmtT
  -- ^ End of an expression
  | EosT
  -- ^ End of source
  deriving (Eq, Ord)

instance Show Token where
  show tok = case tok of
    OBraceT         -> "{"
    CBraceT         -> "}"
    OParenT         -> "("
    CParenT         -> ")"
    OBracketT       -> "["
    CBracketT       -> "]"
    ClassT          -> "class"
    ImportT         -> "import"
    LamT            -> "lam"
    FunT            -> "fun"
    ForT            -> "for"
    InT             -> "as"
    IfT             -> "if"
    ElseT           -> "else"
    CommaT          -> ","
    AssignT         -> "="
    ShellCommandT _ -> "'shell command'"
    RegexExprT _    -> "'regex'"
    NameIdT _       -> "'identifier'"
    ClassIdT _      -> "'class name'"
    LitTextT _      -> "'literal string'"
    NumT _          -> "'number'"
    DecimalT _      -> "'double'"
    BoolT _         -> "'boolean'"
    NoneT           -> "none"
    OperatorT _     -> "'operator'"
    ICommandT _ _   -> "'interpreter command'"
    SkipT           -> "'skip'"
    EndStmtT        -> "'endStmt'"
    DedentT _       -> "'dedent'"
    EosT            -> "'eof'"
