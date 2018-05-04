module Compiler.Token.Types where

import           Data.Text

-- type Parser = Parsec Void String

-- data TokenInfo = TokenInfo
--   { startLoc :: (Int, Int)
--   , tok      :: Token
--   , fragment :: Text
--   , endLoc   :: (Int, Int)
--   } deriving (Eq, Ord, Show)

-- instance Enum TokenInfo where
--   toEnum tokInfo = toEnum $ tok tokInfo
--   fromEnum tokInfo = fromEnum $ tok tokInfo

-- ! Tokens available
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
  -- ^ "as" keyword
  | IfT
  -- ^ "if" keyword
  | ElseT
  -- ^ "else" keyword
  | CommaT
  -- ^ ","
  | AssignT
  -- ^ "="
  | ShellCommandT Text
  -- ^ "!shell_command_name arg1 arg" to execute a shell command
  | RegexExprT Text
  -- ^ "r/Project$digit${3,9}/"
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
  | EosT
  -- ^ End of source
  deriving (Show, Eq, Ord)

