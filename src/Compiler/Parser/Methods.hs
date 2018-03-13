{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Methods where

import           Text.Parsec
import Data.Functor

import Compiler.Ast
import Compiler.Token.Types
import Compiler.Token.Methods


parseInterpreter :: Parsec [Token] () Repl
parseInterpreter = choice
  [ try $ exitT $> Command "exit" []
  , try $ helpT $> Command "help" []
  , try $ multilineOpenT $> Command ":{" []
  , try $ multilineCloseT $> Command ":}" []
  , Code <$> try parseExp
  ]


parseExp :: Parsec [Token] () (Expression ())
parseExp = choice
  [ try parseApply
  , try parseIfElse
  , (\txt -> Factor (AStr txt) ()) <$> try litTextT
  , (\num -> Factor (ANum num) ()) <$> try numberT
  -- , Regex () <$> try regexExprT
  , parensExp
  ]


parseIfElse :: Parsec [Token] () (Expression ())
parseIfElse = do
  ifT
  expr <- parseExp
  progTrue <- between oBraceT cBraceT parseExp
  elseT
  progFalse <- between oBraceT cBraceT parseExp
  return $ IfElse expr progTrue progFalse ()


parseAssign :: Parsec [Token] () (Expression ())
parseAssign = do
  varName <- nameIdT
  assignT
  expr <- parseExp
  return $ VarDecl varName expr ()

parseApply :: Parsec [Token] () (Expression ())
parseApply = do
  name <- nameIdT
  params <- parseExp `sepBy` commaT
  return $ Apply name params ()


parensExp :: Parsec [Token] () (Expression ())
parensExp = between oParenT cParenT parseExp

parseIf :: Parsec [Token] () (Expression ())
parseIf = do
  ifT
  expr <- parseExp
  prog <- between oBraceT cBraceT parseExp
  return $ If expr prog ()

parseFor :: Parsec [Token] () (Expression ())
parseFor = do
  forT
  expr <- parseExp
  asT
  nameVar <- nameIdT
  prog <- between oBraceT cBraceT parseExp
  return $ For nameVar expr prog ()

{-
parseMkScope :: Parsec [Token] () (Statements ())
parseMkScope = do
  expr <- parseExp
  prog <- between oBraceT cBraceT parseLanguage
  return $ MkScope () expr prog

parseStatements :: Parsec [Token] () (Expression ())
parseStatements = choice
  [ try parseFor
  --, try parseIf
  , try parseAssign
  , try parseMkScope
  , Expr <$> try parseExp
  ]
-}

