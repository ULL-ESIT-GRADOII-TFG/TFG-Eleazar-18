module Compiler.Token.Methods where

import           Data.Char
import           Data.Functor
import           Data.Text
import           Text.Parsec
import           Text.Parsec.Pos

import Compiler.Token.Types

parseTokens :: Parsec Text u [Token]
parseTokens = whitespace *> many (whitespace >> parseToken) <* eof

whitespace :: Parsec Text u ()
whitespace = skipMany (satisfy isSpace)

parseInterpreterTokens :: Parsec Text u [Token]
parseInterpreterTokens = whitespace *> many (whitespace >> parseTokenInterpreter) <* eof

parseTokenInterpreter :: Parsec Text u Token
parseTokenInterpreter = choice
  [ try $ string ":help" $> HelpT
  , try $ string ":quit" $> ExitT
  , try $ string ":{" $> MultilineOT
  , try $ string ":}" $>  MultilineCT
  , parseToken
  ]

parseToken :: Parsec Text u Token
parseToken = choice
  [ try $ string "as" *> pure AsT
  , try $ string "for" *> pure ForT
  , try $ string "if" *> pure IfT
  , try $ string "else" *> pure ElseT
  , try $ char ',' *> pure CommaT
  , try $ char '(' *> pure OParenT
  , try $ char ')' *> pure CParenT
  , try $ char '{' *> pure OBraceT
  , try $ char '}' *> pure CBraceT
  , try $ char '=' *> pure AssignT
  , try $ char ':' *> pure DoubleDotsT
  , try shellCommand
  , try identifierParser
  , try stringParser
  , try numberParser
  ]

shellCommand :: Parsec Text u Token
shellCommand = do
  _ <- char '!'
  command <- manyTill anyChar (try endOfLine)
  return $ ShellCommandT (pack command)


-- | Parser a simple integer values it can be negative
numberParser :: Parsec Text u Token
numberParser = do
  signe <- optionMaybe (oneOf "+-")
  number <- many1 digit
  case signe of
    Just '+' -> return $ NumT $ read number
    Just '-' -> return $ NumT $ negate $ read number
    Just _   -> error "No possible case"
    Nothing  ->  return $ NumT $ read number


-- | Parse a string
stringParser :: Parsec Text u Token
stringParser = do
  _ <- char '"'
  litText <- manyTill anyChar (try (char '"'))
  return $ LitTextT $ pack litText


-- | Parse identifier like methods call, variable names...
identifierParser :: Parsec Text u Token
identifierParser = do
  fLetter <- letter
  restName <- many alphaNum
  return $ NameIdT (pack (fLetter:restName))

type TokenParser a = Parsec [Token] () a

match :: Token -> TokenParser ()
match tok =
  token
    show
    (\_tok' -> newPos "TODO" 0 0)
    (\tok' -> if tok == tok' then Just () else Nothing)

cBraceT :: TokenParser ()
cBraceT = match CBraceT

oBraceT :: TokenParser ()
oBraceT = match OBraceT

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
  token
    show
    (\_tok -> newPos "TODO" 0 0)
    (\tok ->
      case tok of
        ShellCommandT command -> Just command
        _                    -> Nothing)

oParenT :: TokenParser ()
oParenT = match OParenT

cParenT :: TokenParser ()
cParenT = match CParenT

regexExprT :: TokenParser Text
regexExprT =
  token
    show
    (\_tok -> newPos "TODO" 0 0)
    (\tok ->
      case tok of
        RegexExprT regex -> Just regex
        _                -> Nothing)

nameIdT :: TokenParser Text
nameIdT =
  token
    show
    (\_tok -> newPos "TODO" 0 0)
    (\tok ->
      case tok of
        NameIdT name -> Just name
        _            -> Nothing)

litTextT :: TokenParser Text
litTextT =
  token
    show
    (\_tok -> newPos "TODO" 0 0)
    (\tok ->
      case tok of
        LitTextT name -> Just name
        _             -> Nothing)

commaT :: TokenParser ()
commaT = match CommaT

assignT :: TokenParser ()
assignT = match AssignT

doubleDotsT :: TokenParser ()
doubleDotsT = match DoubleDotsT

numberT :: TokenParser Int
numberT =
  token
    show
    (\_tok -> newPos "TODO" 0 0)
    (\tok ->
      case tok of
        NumT num -> Just num
        _        -> Nothing)

operatorT :: TokenParser Text
operatorT =
  token
    show
    (\_tok -> newPos "TODO" 0 0)
    (\tok ->
      case tok of
        OperatorT op -> Just op
        _            -> Nothing)

exitT :: TokenParser ()
exitT = match ExitT

helpT :: TokenParser ()
helpT = match HelpT

multilineCloseT :: TokenParser ()
multilineCloseT = match MultilineCT

multilineOpenT :: TokenParser ()
multilineOpenT = match MultilineOT

