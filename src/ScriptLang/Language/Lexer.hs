{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module ScriptLang.Language.Lexer where

import           Data.Char
import           Data.List.NonEmpty
import           Data.Monoid
import qualified Data.Set                  as Set
import           Data.Text                 as T
import           Text.Megaparsec           (Parsec)
import qualified Text.Megaparsec           as P
import           Text.Megaparsec.Text      (Parser)

import           ScriptLang.Language.Token

data SrcLoc = SrcLoc
  { startLoc :: (Word, Word)
  , fragment :: Text
  , endLoc   :: (Word, Word)
  } deriving (Eq, Ord, Show)

mkSrcLoc :: P.SourcePos -> P.SourcePos -> Text -> SrcLoc
mkSrcLoc (P.SourcePos _ l c) (P.SourcePos _ ll cc)  text =
  SrcLoc (P.unPos l, P.unPos c) text (P.unPos ll, P.unPos cc)

type TokenInfo = (Token, SrcLoc)

parseTokens :: Parser [TokenInfo]
parseTokens = whitespace *> P.many (whitespace >> parseToken) <* P.eof

whitespace :: Parser ()
whitespace = P.skipMany (P.satisfy isSpace)

parseInterpreterTokens :: Parser [TokenInfo]
parseInterpreterTokens = whitespace *> P.many (whitespace >> parseTokenInterpreter) <* P.eof

token :: String -> Token -> Parser TokenInfo
token val tok = do
  start <- P.getPosition
  text <- P.try $ P.string val
  end <- P.getPosition
  return (tok, mkSrcLoc start end (T.pack text))

charToken :: Char -> Token -> Parser TokenInfo
charToken ch tok = do
  start <- P.getPosition
  _ <- P.try $ P.char ch
  end <- P.getPosition
  return (tok, mkSrcLoc start end (T.singleton ch))

parseTokenInterpreter :: Parser TokenInfo
parseTokenInterpreter = P.choice
  [ token ":help" HelpT
  , token ":quit" ExitT
  , token ":{" MultilineOT
  , token ":}" MultilineCT
  , parseToken
  ]

parseToken :: Parser TokenInfo
parseToken = P.choice
  [ token "as" AsT
  , token "fun" FunT
  , token "obj" ObjectT
  , token "while" WhileT
  , token "for" ForT
  , token "if" IfT
  , token "else" ElseT
  , charToken ',' CommaT
  , charToken '(' OParenT
  , charToken ')' CParenT
  , charToken '{' OBraceT
  , charToken '}' CBraceT
  , charToken '=' AssignT
  , charToken ':' DoubleDotsT
  , P.try regexParser
  , P.try shellCommandParser
  , P.try identifierParser
  , P.try stringParser
  , P.try numberParser
  ]

regexParser :: Parser TokenInfo
regexParser = do
  start <- P.getPosition
  _ <- P.string "r/"
  regex <- T.concat <$> P.some (P.choice
    [ P.try $ fmap pack (P.string "\\/")
    , P.try $ fmap singleton (P.satisfy (/= '/'))
    ])
  _ <- P.char '/'
  end <- P.getPosition
  return (RegexExprT regex, mkSrcLoc start end (T.concat ["r/", regex, "/"]))

shellCommandParser :: Parser TokenInfo
shellCommandParser = do
  start <- P.getPosition
  _ <- P.char '!'
  command <- pack <$> P.manyTill P.anyChar (P.try P.eol)
  end <- P.getPosition
  return (ShellCommandT command, mkSrcLoc start end ("!" <> command))


-- | Parser a simple integer values it can be negative
numberParser :: Parser TokenInfo
numberParser = do
  start <- P.getPosition
  signe <- P.optional (P.oneOf ("+-"::String))
  number <- P.some P.digitChar
  end <- P.getPosition
  case signe of
    Just '+' -> return (NumT $ read number, mkSrcLoc start end ("+" <> pack number))
    Just '-' -> return (NumT $ negate $ read number, mkSrcLoc start end ("-" <> pack number))
    Just _   -> error "No possible case"
    Nothing  -> return (NumT $ read number, mkSrcLoc start end (pack number))


-- | Parse a string
stringParser :: Parser TokenInfo
stringParser = do
  start <- P.getPosition
  _ <- P.char '"'
  litText <- T.concat <$> P.many (P.choice
    [ P.try $ fmap pack (P.string "\\\"")
    , P.try $ fmap singleton (P.satisfy (/= '"'))
    ])
  end <- P.getPosition
  return (LitTextT litText, mkSrcLoc start end ("\"" <> litText <> "\""))


-- | Parse identifier like methods call, variable names...
identifierParser :: Parser TokenInfo
identifierParser = do
  start <- P.getPosition
  fLetter <- P.letterChar
  restName <- P.many P.alphaNumChar
  end <- P.getPosition
  let varname = pack (fLetter:restName)
  return (NameIdT varname, mkSrcLoc start end varname)

updatePosToken
  :: P.Pos                  -- ^ Tab width
  -> P.SourcePos            -- ^ Current position
  -> TokenInfo              -- ^ Current token
  -> (P.SourcePos, P.SourcePos) -- ^ Actual position and incremented position
updatePosToken _width (P.SourcePos n _ _) (_, SrcLoc start _ end) =
    ( P.SourcePos n (P.unsafePos (fst start)) (P.unsafePos (snd start))
    , P.SourcePos n (P.unsafePos (fst end)) (P.unsafePos (snd end)))

instance P.Stream [TokenInfo] where
  type Token [TokenInfo] = TokenInfo

  uncons []     = Nothing
  uncons (t:ts) = Just (t, ts)
  {-# INLINE uncons #-}

  updatePos = const updatePosToken
  {-# INLINE updatePos #-}

type TokenParser a = Parsec P.Dec [TokenInfo] a

match :: Token -> TokenParser ()
match tok =
  P.token
    (\(tok', info) ->
      if tok == tok'
      then Right ()
      else Left (Set.singleton (P.Tokens ((tok',info) :| [])), Set.empty, Set.empty))
    Nothing

cBraceT :: TokenParser ()
cBraceT = match CBraceT

oBraceT :: TokenParser ()
oBraceT = match OBraceT

objT :: TokenParser ()
objT = match ObjectT

funT :: TokenParser ()
funT = match FunT

whileT :: TokenParser ()
whileT = match WhileT

forT :: TokenParser ()
forT = match ForT

asT :: TokenParser ()
asT = match AsT

ifT :: TokenParser ()
ifT = match IfT

elseT :: TokenParser ()
elseT = match ElseT

shellCommandT :: TokenParser Text
shellCommandT =
  P.token
    (\(tok', info) ->
      case tok' of
       ShellCommandT command -> Right command
       _ -> Left (Set.singleton (P.Tokens ((tok', info) :| [])), Set.empty, Set.empty))
    Nothing

oParenT :: TokenParser ()
oParenT = match OParenT

cParenT :: TokenParser ()
cParenT = match CParenT

regexExprT :: TokenParser Text
regexExprT =
  P.token
    (\(tok', info) ->
      case tok' of
       RegexExprT regex -> Right regex
       _ -> Left (Set.singleton (P.Tokens ((tok', info) :| [])), Set.empty, Set.empty))
    Nothing

nameIdT :: TokenParser Text
nameIdT =
  P.token
    (\(tok', info) ->
      case tok' of
        NameIdT name -> Right name
        _ -> Left (Set.singleton (P.Tokens ((tok', info) :| [])), Set.empty, Set.empty))
    Nothing

litTextT :: TokenParser Text
litTextT =
  P.token
    (\(tok', info) ->
      case tok' of
        LitTextT name -> Right name
        _ -> Left (Set.singleton (P.Tokens ((tok', info) :| [])), Set.empty, Set.empty))
    Nothing

commaT :: TokenParser ()
commaT = match CommaT

assignT :: TokenParser ()
assignT = match AssignT

doubleDotsT :: TokenParser ()
doubleDotsT = match DoubleDotsT

numberT :: TokenParser Int
numberT =
  P.token
    (\(tok', info) ->
      case tok' of
        NumT name -> Right name
        _ -> Left (Set.singleton (P.Tokens ((tok', info) :| [])), Set.empty, Set.empty))
    Nothing

operatorT :: TokenParser Text
operatorT =
  P.token
    (\(tok', info) ->
      case tok' of
        OperatorT name -> Right name
        _ -> Left (Set.singleton (P.Tokens ((tok', info) :| [])), Set.empty, Set.empty))
    Nothing

exitT :: TokenParser ()
exitT = match ExitT

helpT :: TokenParser ()
helpT = match HelpT

multilineCloseT :: TokenParser ()
multilineCloseT = match MultilineCT

multilineOpenT :: TokenParser ()
multilineOpenT = match MultilineOT
