{
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- {-# OPTIONS_HADDOCK prune #-}
module Compiler.Token.Lexer where
{-
  This module needs several modifications:
    - On start multiline token like string, regex, shellcommands... Add initial token <code_st>
    - Tokens should be delimited with start and en position with its file offset positions
    - Modify monadUserState, create own monad to a better error manipulation
      + This means touch alexMonadScan which use alexScan create by alex
      + Add all required functions to it
    - All this modifications could be improve adding several features to alex
      + QuasiQuotes to generate. And convert alex into a library. Exporting the
        differents wrappers.
-}


import qualified Data.Vector as V
import qualified Data.Text as T

import Compiler.Token.Types

}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [_a-zA-Z]
$alphaDigit = [_a-zA-Z0-9]
$operators = [\\\/\>\<\!\@\=\$\%\&\?\+\-\*\.\^\|]

@nameId = $alpha $alphaDigit*
@classId = [A-Z] @nameId?

@emptyLines = ([\ \t]*("#".*)?\n)+
@comments = ([\ \t]*"#".*\n)+
@number = $digit+
@decimal = @number "." $digit+


tokens :-

  <0> {
    :.+           { mkL' (\text -> let (command:args) = T.words $ T.drop 1 text in ICommandT command args) }
    $white*       { begin code_st }
  }

  <empty_lines> {
    @emptyLines    { begin inc_indent}
  }

  <inc_indent> {
    [\ \t]*       { newIndent `andBegin` code_st }
  }

  <code_st> {
    "#".*         ;  -- Comment Line
    [\ \t]+       ;
    ":"           { mkL OBraceT `andBegin` empty_lines }
    \n            { begin dec_indent }
    fun           { mkL FunT }
    lam           { mkL LamT }
    in            { mkL InT }
    for           { mkL ForT }
    if            { mkL IfT }
    else          { mkL ElseT }
    class         { mkL ClassT }
    -- import        { mkL ImportT }
    "["           { mkL OBracketT }
    "]"           { mkL CBracketT }
    "{"           { mkL OBraceT }
    "}"           { mkL CBraceT }
    "("           { mkL OParenT }
    ")"           { mkL CParenT }
    ","           { mkL CommaT }
    ";"           { mkL EndStmtT }
    "="           { mkL AssignT }
    "none"        { mkL NoneT }
    "true"        { mkL (BoolT True) }
    "false"       { mkL (BoolT False) }
    \"            { begin string }
    \!"$"         { mkL (OperatorT $ T.pack "!") `andBegin` shell }
    "$"           { begin shell }
    \!\$\"        { mkL (OperatorT $ T.pack "!") `andBegin` shell_alternative }
    \$\"          { begin shell_alternative }
    r\"           { begin regex }
    $operators+   { mkL' OperatorT }
    @number       { mkL' (NumT . read . T.unpack) }
    @decimal      { mkL' (DecimalT . read . T.unpack) }
    @classId      { mkL' ClassIdT }
    @nameId       { mkL' NameIdT }
  }

  <dec_indent> {
    @emptyLines   ;
    [\ \t]*       { checkDecrement `andBegin` code_st }
  }

  <string> {
    \\\"          { skipJustAdd "\"" }
    \\n           { skipJustAdd "\n" }
    \\t           { skipJustAdd "\t" }
    \n            { skipJustAdd "\n" }
    \t            { skipJustAdd "\t" }
    [^\"]         { addToInnerString }
    \"            { generateLexerFromInner LitTextT `andBegin` code_st }
  }

  <shell> {
    "\$"           { skipJustAdd "$" }
    \\n           { skipJustAdd "\n" }
    \\t           { skipJustAdd "\t" }
    \n            { skipJustAdd "\n" }
    \t            { skipJustAdd "\t" }
    [^\$]          { addToInnerString }
    "$"            { generateLexerFromInner ShellCommandT `andBegin` code_st }
  }

  <shell_alternative> {
    \\\"           { skipJustAdd "/" }
    \\n           { skipJustAdd "\n" }
    \\t           { skipJustAdd "\t" }
    \n            { skipJustAdd "\n" }
    \t            { skipJustAdd "\t" }
    [^\"]          { addToInnerString }
    \"             { generateLexerFromInner ShellCommandT `andBegin` code_st }
  }

  <regex> {
    \\\"           { skipJustAdd "/" }
    \\n            { skipJustAdd "\n" }
    \\t            { skipJustAdd "\t" }
    [^\"]          { addToInnerString }
    \"             { generateLexerFromInner RegexExprT `andBegin` code_st }
  }
{

-- | Used to check if there more code to come. For interpreter to enter
-- several lines
data Tokenizer
  = Partial (V.Vector Lexeme)
  | Complete (V.Vector Lexeme)
  deriving (Show, Eq)

-- | Get tokens from tokenizer
getTokens :: Tokenizer -> V.Vector Lexeme
getTokens (Partial tokens) = tokens
getTokens (Complete tokens) = tokens

-- A Lexeme
data Lexeme = L
  { posn :: AlexPosn
  , tokn :: Token
  } deriving Eq

instance Show Lexeme where
  show (L _pos tok) = show tok

-- | Internal use. Used to take account of indent level
data AlexUserState = AlexUserState
  { indentStack :: [Int]
  , generatedString :: String
  } deriving Show

-- | Internal use.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [] ""

-- | Internal use.
alexEOF = return (L (AlexPn 0 0 0) EosT)

-- | Internal use.
mkL :: Token -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,_,_) _ = return (L p c)

-- | Internal use.
mkL' :: (T.Text -> Token) -> AlexInput -> Int -> Alex Lexeme
mkL' mkTok (p,_,_,str) len = return (L p (mkTok . T.pack $ take len str))

-- | Remove delimitators from string
skipDels :: T.Text -> T.Text
skipDels = T.dropEnd 1 . T.drop 1

-- | FIX: Position token
addToInnerString :: AlexInput -> Int -> Alex Lexeme
addToInnerString input@(_,_,_,str) len = do
  userState <- alexGetUserState
  alexSetUserState $ userState { generatedString = (generatedString userState) ++ (take len str) }
  mkL SkipT input len

skipJustAdd :: String -> AlexInput -> Int -> Alex Lexeme
skipJustAdd val input len = do
  userState <- alexGetUserState
  alexSetUserState $ userState { generatedString = generatedString userState ++ val }
  mkL SkipT input len

generateLexerFromInner :: (T.Text -> Token) -> AlexInput -> Int -> Alex Lexeme
generateLexerFromInner mkTok input len = do
  userState <- alexGetUserState
  alexSetUserState $ userState { generatedString = "" }
  mkL (mkTok . T.pack $ generatedString userState) input len

-- | Internal use. Generate a new ident in the tokenizer
newIndent :: AlexInput -> Int -> Alex Lexeme
newIndent input len = do
  indentStackOld <- alexGetUserState
  alexSetUserState $ indentStackOld { indentStack = len : (indentStack indentStackOld) }
  mkL SkipT input len

-- | Internal use. Check to add correspond token to decrement
checkDecrement :: AlexInput -> Int -> Alex Lexeme
checkDecrement input len = do
  userState <- alexGetUserState
  case indentStack userState of
    []
      | 0 == len -> mkL EndStmtT input len
      | otherwise -> mkL SkipT input len

      {-if len == 0 then do
        alexSetUserState alexInitUserState
        mkL CBraceT input len
      else do
        userState <- alexGetUserState
        alexSetUserState (userState { indentStack = [] })
        mkL SkipT input len-}
    stack@(x:_)
      | x == len  -> mkL EndStmtT input len
      | x < len   -> mkL SkipT input len
      | otherwise -> do -- getDedent len stack
          let (removes, rest) = break (<= len) stack
          userState <- alexGetUserState
          alexSetUserState (userState { indentStack = rest })
          mkL (DedentT $ length removes) input len


-- | Run tokenizer over string
scanner
  :: Bool -- ^ Use to close indentation, by default it should be true. (Used in
          --   interpreter to check multiline input)
   -> String -- ^ Source text code
   -> Either String Tokenizer
scanner autoClose str = runAlex str $ do
  let loop vec = do
        lexeme@(L i tok) <- alexMonadScan
        case tok of
          SkipT -> loop vec
          DedentT n -> loop (vec `mappend` V.replicate n (L i CBraceT))
          EosT ->
            if autoClose then do
              userState <- alexGetUserState
              let n = length $ indentStack userState
              return (vec `mappend` V.replicate n (L i CBraceT))
            else
              return vec
          _ -> loop (vec `V.snoc` lexeme)
  tokens <- loop mempty
  if V.null tokens then
    return $ Complete tokens
  else if tokn (V.last tokens) == OBraceT then -- TODO: Add more Partial cases strings, apply...
    return $ Partial tokens
  else
    return $ Complete tokens

}
