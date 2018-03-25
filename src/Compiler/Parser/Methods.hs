{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Methods where

import           Text.Parsec
import Data.Functor

import Compiler.Ast
import Compiler.Parser.Types
import Compiler.Token.Methods


parseInterpreter :: TokenParser Repl
parseInterpreter = choice
  [ try $ exitT $> Command "exit" []
  , try $ helpT $> Command "help" []
  , Code <$> try parseSeqExpr
  ]


parseExp :: TokenParser (Expression TokenInfo)
parseExp = choice $ map try
  [ parseFunDecl
  , parseLam
  , parseAssign
  , parseApply
  , parseIf
  , parseIfElse
  , parseIdentifier
  , (\txt -> Factor (AStr txt) TokenInfo) <$> litTextT
  , (\num -> Factor (ANum num) TokenInfo) <$> numberT
  -- , Regex () <$> try regexExprT
  , parensExp
  ]

parseSeqExpr :: TokenParser (Expression TokenInfo)
parseSeqExpr = do
  exprs <- many1 parseExp
  return $ SeqExpr exprs TokenInfo

parseFunDecl :: TokenParser (Expression TokenInfo)
parseFunDecl = do
  funT
  funName <- nameIdT
  params <- many nameIdT
  prog <- between oBraceT cBraceT parseSeqExpr
  return $ VarDecl funName (FunDecl params prog TokenInfo) TokenInfo

parseLam :: TokenParser (Expression TokenInfo)
parseLam = do
  lamT
  params <- many nameIdT
  prog <- between oBraceT cBraceT parseSeqExpr
  return $ FunDecl params prog TokenInfo

parseAssign :: TokenParser (Expression TokenInfo)
parseAssign = do
  varName <- nameIdT
  assignT
  expr <- parseSeqExpr
  return $ VarDecl varName expr TokenInfo

parseIf :: TokenParser (Expression TokenInfo)
parseIf = do
  ifT
  expr <- parseSeqExpr
  prog <- between oBraceT cBraceT parseSeqExpr
  return $ If expr prog TokenInfo

parseIfElse :: TokenParser (Expression TokenInfo)
parseIfElse = do
  ifT
  expr <- parseSeqExpr
  progTrue <- between oBraceT cBraceT parseSeqExpr
  elseT
  progFalse <- between oBraceT cBraceT parseSeqExpr
  return $ IfElse expr progTrue progFalse TokenInfo

parseFor :: TokenParser (Expression TokenInfo)
parseFor = do
  forT
  expr <- parseSeqExpr
  inT
  nameVar <- nameIdT
  prog <- between oBraceT cBraceT parseSeqExpr
  return $ For nameVar expr prog TokenInfo

parseApply :: TokenParser (Expression TokenInfo)
parseApply = do
  name <- nameIdT
  params <- between oParenT cParenT (parseSeqExpr `sepBy` commaT)
  return $ Apply name params TokenInfo

-- TODO: Ver como configurar el tema de las precedencia de los operadores

parseIdentifier :: TokenParser (Expression TokenInfo)
parseIdentifier = do
  name <- nameIdT
  return $ Identifier name TokenInfo

parensExp :: TokenParser (Expression TokenInfo)
parensExp = between oParenT cParenT parseSeqExpr
