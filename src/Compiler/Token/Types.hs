{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Compiler.Token.Types where
{-
  This file contains several Types definitions extracted from alex Template.
  Several of them, are modified for this specific use
-}
import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy       as ByteString
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Data.Word                  (Word8)

import           Compiler.Utils

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
  | UseT
  -- ^ "use" keyword
  | CdT
  -- ^ "cd" keyword
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
  deriving (Eq, Ord, Show)

-- instance Show Token where
--   show tok = case tok of
--     OBraceT         -> "{"
--     CBraceT         -> "}"
--     OParenT         -> "("
--     CParenT         -> ")"
--     OBracketT       -> "["
--     CBracketT       -> "]"
--     ClassT          -> "class"
--     LamT            -> "lam"
--     FunT            -> "fun"
--     ForT            -> "for"
--     InT             -> "as"
--     IfT             -> "if"
--     ElseT           -> "else"
--     CommaT          -> ","
--     AssignT         -> "="
--     ShellCommandT _ -> "'shell command'"
--     RegexExprT _    -> "'regex'"
--     NameIdT _       -> "'identifier'"
--     ClassIdT _      -> "'class name'"
--     LitTextT _      -> "'literal string'"
--     NumT _          -> "'number'"
--     DecimalT _      -> "'double'"
--     BoolT _         -> "'boolean'"
--     NoneT           -> "none"
--     OperatorT _     -> "'operator'"
--     ICommandT _ _   -> "'interpreter command'"
--     SkipT           -> "'skip'"
--     EndStmtT        -> "'endStmt'"
--     DedentT _       -> "'dedent'"
--     EosT            -> "'eof'"


-- -----------------------------------------------------------------------------
-- * Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

-- | Token position used
data TokPos = TokPos
  { _offset :: !Int -- ^ Offset
  , _line :: !Int -- ^ Line
  , _column :: !Int -- ^ Column
  } deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- * The input type

-- | Default input type
-- - current position,
-- - previous char
-- - current input string
-- - bytes consumed so far
type AlexInput = (TokPos, Char, ByteString.ByteString, Int64)

type Byte = Word8

-- | These values are returned by generated functions `LexerGen`
data AlexReturn action
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int action

-- | Used to determine which kind of string is been saved into the internal
-- state.
data KindString
  = Shell
  | ShellStr
  | Str
  | Reg
  | None
  deriving (Show, Eq)

-- | Internal State of tokenizer, alex prefixed belongs to alex template
data TokenizerSt = TokenizerSt
  { alex_pos         :: !TokPos  -- ^ position at current input location
  , alex_bpos        :: !Int64     -- ^ bytes consumed so far
  , alex_inp         :: ByteString.ByteString      -- ^ the current input
  , alex_chr         :: !Char      -- ^ the character before the input
  , alex_scd         :: !Int        -- ^ the current startcode

  , _stackGroups     :: [(Token, TokPos)]
  -- ^ Allows to stack parents and brackets
  , _initialPos      :: !TokPos
  -- ^ Use to get initial position on possible multilines tokens
  , _indentStack     :: [Int64]
  -- ^ Used to control Indent behavior
  , _generatedString :: T.Text
  -- ^ Allows save a temporary string before create final token
  , _kindGenString   :: KindString
  -- ^ Determine ´_generateString´ type token
  } deriving (Show)

-- | Token info parser
data ErrorTokenizer
  = ErrorTokenizer T.Text
  | ExpectedOtherToken
    { expectedOn :: TokPos
    , expectedKind :: [String]
    }
  | WrongClosedGroup
    { expected :: Token
    , found :: Maybe (Token, TokPos)
    , closing :: (Token, TokPos) }
  deriving (Show, Eq)

type TokenizerM a = StateT TokenizerSt (Except ErrorTokenizer) a

type AlexAction tok = AlexInput -> Int64 -> TokenizerM tok

-- | Token with position information
data Lexeme = L
  { start :: TokPos
  , end :: TokPos
  , tokn :: Token
  } deriving Eq

-- TODO: Pretty
instance Show Lexeme where
  show (L _start _end tok) = show tok

-- | Used to check if there more code to come. For interpreter to enter
-- several lines
data Tokenizer
  = Partial (V.Vector Lexeme)
  | Complete (V.Vector Lexeme)
  deriving (Show, Eq)

-- * Generated Lenses
makeSuffixLenses ''TokenizerSt
makeSuffixLenses ''TokPos
