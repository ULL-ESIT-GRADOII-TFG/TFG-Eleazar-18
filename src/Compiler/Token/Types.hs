module Compiler.Token.Types where

import Data.Text


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
  | FunT
  -- ^ "fun" keyword
  | ObjectT
  -- ^ "obj" keyword
  | ForT
  -- ^ "for" keyword
  | AsT
  -- ^ "as" keyword
  | IfT
  -- ^ "if" keyword
  | ElseT
  -- ^ "else" keyword
  | WhileT
  -- ^ "while" keyword
  | ShellCommandT Text
  -- ^ "!shell_command_name arg1 arg" to execute a shell command
  | OParenT
  -- ^ "("
  | CParenT
  -- ^ ")"
  | RegexExprT Text
  -- ^ "r/Project$digit${3,9}/"
  | NameIdT Text
  -- ^ "hello"
  | LitTextT Text
  -- ^ A literal string
  | CommaT
  -- ^ ","
  | AssignT
  -- ^ "="
  | DoubleDotsT
  -- ^ ":"
  | NumT Int
  -- ^ Represents a integer
  | OperatorT Text
  -- ^ Operators are symbols like +,-,*,/,++....
  | ExitT
  | HelpT
  | MultilineOT
  | MultilineCT
  deriving (Show, Eq, Ord)

