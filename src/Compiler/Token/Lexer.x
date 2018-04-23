{
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Compiler.Token.Lexer where

import qualified Data.Vector as V
import qualified Data.Text as T

import Compiler.Token.Types

}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [_a-zA-Z]
$alphaDigit = [_a-zA-Z0-9]
$operators = [\>\<\!\@\=\/\$\%\&\?\+\-\*\.\^]

@nameId = $alpha $alphaDigit*
@classId = [A-Z] @nameId?

@emptyLines = ([ \t]*\n)+
@number = \-? $digit+
@decimal = @number "." $digit+
-- TODO: skip \$
@shellCommand = "$" (~[\n\$])* "$"
-- TODO: skip \/
@regex = "/" (~[\n\/])* "/"
-- TODO: skip \" and others
@string = \" (~[\"\n])* \"


tokens :-

  <0> {
    :.+           { mkL' (\text -> ICommandT (T.drop 1 text) []) }
    $white*       { begin code_st }
  }

  <inc_indent> {
    @emptyLines   ;
    [\ \t]*       { newIndent `andBegin` code_st }
  }

  <code_st> {
    "#".*         ;  -- Comment Line
    [\ \t]+       ;
    ":"           { mkL OBraceT `andBegin` inc_indent }
    \n            { begin dec_indent }
    @emptyLines   ;
    fun           { mkL FunT }
    lam           { mkL LamT }
    in            { mkL InT }
    for           { mkL ForT }
    if            { mkL IfT }
    else          { mkL ElseT }
    class         { mkL ClassT }
    import        { mkL ImportT }
    "["           { mkL OBracketT }
    "]"           { mkL CBracketT }
    "{"           { mkL OBraceT }
    "}"           { mkL CBraceT }
    "("           { mkL OParenT }
    ")"           { mkL CParenT }
    ","           { mkL CommaT }
    "="           { mkL AssignT }
    "true"        { mkL (BoolT True) }
    "false"       { mkL (BoolT False) }
    @shellCommand { mkL' ShellCommandT }
    @regex        { mkL' RegexExprT }
    @string       { mkL' LitTextT }
    @number       { mkL' (NumT . read . T.unpack) }
    @decimal      { mkL' (DecimalT . read . T.unpack) }
    $operators+   { mkL' OperatorT }
    @classId      { mkL' ClassIdT }
    @nameId       { mkL' NameIdT }
  }


  <dec_indent> [\ \t]* { checkDecrement `andBegin` code_st }

  -- \"      { begin string }
  -- <string> [^\"]*          { string }
  -- <string> \"             { begin code_st }


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
  } deriving (Show, Eq)

-- | Internal use. Used to take account of indent level
data AlexUserState = AlexUserState
  { indentStack :: [Int]
  }

-- | Internal use.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState []


-- | Internal use.
alexEOF = return (L (AlexPn 0 0 0) EosT)

-- | Internal use.
mkL :: Token -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,_,_) _ = return (L p c)

-- | Internal use.
mkL' :: (T.Text -> Token) -> AlexInput -> Int -> Alex Lexeme
mkL' mkTok (p,_,_,str) len = return (L p (mkTok . T.pack $ take len str))


-- | Internal use. Generate a new ident in the tokenizer
newIndent :: AlexInput -> Int -> Alex Lexeme
newIndent input len = do
  indentStackOld <- alexGetUserState
  alexSetUserState $ AlexUserState { indentStack = len : (indentStack indentStackOld) }
  mkL SkipT input len

-- | Internal use. Check to add correspond token to decrement
checkDecrement :: AlexInput -> Int -> Alex Lexeme
checkDecrement input len = do
  userState <- alexGetUserState
  case indentStack userState of
    []            -> alexError "No hay indentacion anterior" -- mkL SkipT input len
    stack@(x:_)
      | x == len  -> mkL SkipT input len
      | x < len   -> mkL SkipT input len
      | otherwise -> getDedent len stack
  where
    getDedent :: Int -> [Int] -> Alex Lexeme
    getDedent currIndent [] =
      if currIndent == 0 then do
        alexSetUserState (AlexUserState [])
        mkL CBraceT input len
      else mkL SkipT input len
    getDedent currIndent xs = do
      let (removes, rest) = break (<= currIndent) xs
      alexSetUserState (AlexUserState rest)
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
  else if tokn (V.last tokens) == OBraceT then
    return $ Partial tokens
  else
    return $ Complete tokens

}
