module ScriptLang.Language.Parser where

import           Text.Megaparsec           (Parsec)
import qualified Text.Megaparsec           as P

import           ScriptLang.Language.AST
import           ScriptLang.Language.Lexer


parseInterpreter :: Parsec P.Dec [TokenInfo] (REPL ())
parseInterpreter = P.choice
  [ P.try $ exitT *> pure Exit
  , P.try $ helpT *> pure Help
  , P.try $ multilineOpenT *> pure MultilineOpen
  , P.try $ multilineCloseT *> pure MultilineClose
  , Code <$> P.try parseLanguage
  ]

parseLanguage :: Parsec P.Dec [TokenInfo] (Program ())
parseLanguage = P.many parseStatements

parseStatements :: Parsec P.Dec [TokenInfo] (Statement ())
parseStatements = P.choice
  [ P.try parseFor
  , P.try parseIf
  , P.try parseIfElse
  -- , P.try parseAssign
  , P.try parseMkScope
  , SExpr () <$> P.try parseExp
  ]

parseIf :: Parsec P.Dec [TokenInfo] (Statement ())
parseIf = do
  ifT
  expr <- parseExp
  prog <- P.between oBraceT cBraceT parseLanguage
  return $ SIf () expr prog

parseIfElse :: Parsec P.Dec [TokenInfo] (Statement ())
parseIfElse = do
  ifT
  expr <- parseExp
  progTrue <- P.between oBraceT cBraceT parseLanguage
  elseT
  progFalse <- P.between oBraceT cBraceT parseLanguage
  return $ SIfElse () expr progTrue progFalse

parseFunction :: Parsec P.Dec [TokenInfo] (Statement ())
parseFunction = do
  funT
  funName <- nameIdT
  oParenT
  args <- P.sepBy nameIdT commaT
  prog <- parseLanguage
  return $ SFunction () funName args prog

parseFor :: Parsec P.Dec [TokenInfo] (Statement ())
parseFor = do
  forT
  expr <- parseExp
  mNameVar <- P.optional (asT >> nameIdT)
  prog <- P.between oBraceT cBraceT parseLanguage
  return $ SFor () expr mNameVar prog

parseMkScope :: Parsec P.Dec [TokenInfo] (Statement ())
parseMkScope = do
  expr <- parseExp
  prog <- P.between oBraceT cBraceT parseLanguage
  return $ SMkScope () expr prog

  {-
parseAssign :: Parsec P.Dec [TokenInfo] (Statement ())
parseAssign = do
  varName <- nameIdT
  assignT
  expr <- parseExp
  return $ Assign () varName expr
  -}

parseApply :: Parsec P.Dec [TokenInfo] Exp
parseApply = do
  name <- nameIdT
  params <- parseExp `P.sepBy` commaT
  return $ EApply [name] params

parseExp :: Parsec P.Dec [TokenInfo] Exp
parseExp = P.choice
  [ P.try parseApply
  , EString <$> P.try litTextT
  , EInt <$> P.try numberT
  , ERegex <$> P.try regexExprT
  , parensExp
  ]

parensExp :: Parsec P.Dec [TokenInfo] Exp
parensExp = P.between oParenT cParenT parseExp
