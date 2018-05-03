{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Methods where

import           Text.Parsec
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M

import Compiler.Ast
import Compiler.Prelude.Methods (operatorsPrecedence)
import Compiler.Prelude.Types (Assoc(..))
import Compiler.Parser.Types
import Compiler.Token.Methods
import Compiler.Token.Lexer (Lexeme)

{-
  TODO: Generate an useful `TokenInfo`
  TODO: Parse Vector
 -}

parserLexer :: SourceName -> V.Vector Lexeme -> Either ParseError Repl
parserLexer = parse parseInterpreter

parseInterpreter :: TokenParser Repl
parseInterpreter = choice
  [ uncurry Command <$> try iCommandT
  , Code <$> try parseStatements
  ] <* eof

parseStatements :: TokenParser [Statement TokenInfo]
parseStatements = many (choice $ map
  try
  [ parseClassStatement
  , parseImportStatement
  , (`Expr` TokenInfo) <$> parseSeqExpr
  ]) <* eof

parseImportStatement :: TokenParser (Statement TokenInfo)
parseImportStatement = do
  importT
  text <- litTextT
  return $ Import text TokenInfo

parseClassStatement :: TokenParser (Statement TokenInfo)
parseClassStatement = do
  classT
  nameClass <- classIdT
  bodyClass <- between oBraceT cBraceT parseBodyClass
  return $ Class nameClass bodyClass TokenInfo

parseBodyClass :: TokenParser (Expression TokenInfo)
parseBodyClass = choice $ map try [parseFunDecl, parseAssign, parseIdentifier]

parseExp :: TokenParser (Expression TokenInfo)
parseExp = choice $ map
  try
  [ parseUnaryOperators
  , parseFunDecl
  , parseLam
  , parseApply
  , parseIf
  , parseIfElse
  , parseAssign
  , parseOperators
  ]

parseSeqExpr :: TokenParser (Expression TokenInfo)
parseSeqExpr = do
  exprs <- many1 parseExp
  return $ SeqExpr exprs TokenInfo

parseFunDecl :: TokenParser (Expression TokenInfo)
parseFunDecl = do
  funT
  funName <- nameIdT
  params  <- many nameIdT
  prog    <- between oBraceT cBraceT parseSeqExpr
  return $ VarDecl (Simple funName TokenInfo) (FunDecl params prog TokenInfo) TokenInfo

parseLam :: TokenParser (Expression TokenInfo)
parseLam = do
  lamT
  params <- many nameIdT
  prog   <- between oBraceT cBraceT parseSeqExpr
  return $ FunDecl params prog TokenInfo

parseAssign :: TokenParser (Expression TokenInfo)
parseAssign = do
  varName <- parseAccessor
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
  expr     <- parseSeqExpr
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
  prog    <- between oBraceT cBraceT parseSeqExpr
  return $ For nameVar expr prog TokenInfo

parseUnaryOperators :: TokenParser (Expression TokenInfo)
parseUnaryOperators = do
  operator <- operatorT
  expr <- parseExp
  return $ Apply (Operator operator TokenInfo) [expr] TokenInfo

parseApply :: TokenParser (Expression TokenInfo)
parseApply = do
  name   <- parseAccessor
  params <- between oParenT cParenT (parseSeqExpr `sepBy` commaT)
  return $ Apply name params TokenInfo

checkOperator :: Int -> TokenParser T.Text
checkOperator level = do
  op <- operatorT
  case M.lookup op operatorsPrecedence of
    Just (levelOP, _assoc) ->
      if levelOP == level
        then return op
        else parserFail ("Operator in wrong level "++ show level)
    Nothing -> parserFail "No valid operator"

levels :: [Int] -> TokenParser (Expression TokenInfo)
levels = foldl level parseFactor
  where
    level nextLevel leveln = do
      expr <- nextLevel
      listLevel <- many $ try ((,) <$> checkOperator leveln <*> nextLevel)
      return $ treeOperators expr listLevel

treeOperators :: Expression TokenInfo -> [(T.Text, Expression TokenInfo)] -> Expression TokenInfo
treeOperators expr [] = expr
treeOperators expr list@((op, _):_) =
  case M.lookup op operatorsPrecedence of
    Just (_, LeftAssoc) ->
      foldl (\acc (_, expr') ->
        Apply (Operator op TokenInfo) [acc, expr'] TokenInfo) expr list
    Just (_, RightAssoc) ->
      foldr1 (\expr' acc ->
        Apply (Operator op TokenInfo)  [expr', acc] TokenInfo) (expr: map snd list)
    Nothing -> error "Operator not found. Internal error"

parseOperators :: TokenParser (Expression TokenInfo)
parseOperators = levels [10,9..1]

parseAccessor :: TokenParser (Accessor TokenInfo)
parseAccessor = choice $ map try
  [ do
    name <- nameIdT
    operatorT' "."
    rest <- parseAccessor
    return $ Dot name rest TokenInfo
  , do
    name <- nameIdT
    expr <- between oBraceT cBraceT parseExp
    mRest <- optionMaybe parseAccessor
    return $ Bracket name expr mRest TokenInfo
  , (`Simple` TokenInfo) <$> nameIdT
  ]

parseIdentifier :: TokenParser (Expression TokenInfo)
parseIdentifier = do
  name <- parseAccessor
  return $ Identifier name TokenInfo

parensExp :: TokenParser (Expression TokenInfo)
parensExp = between oParenT cParenT parseSeqExpr

-- parseFactorMethod :: TokenParser (FreeT Instruction StWorld Object)
-- parseFactorMethod =
--   factor <- parseFactor  -- Object
--   accessor <- parseAccessor -- Accessor
--   idFun <- findMethod factor accesor  -- ObjectFunc
--   callCommand idFun [factor]

parseFactor :: TokenParser (Expression TokenInfo)
parseFactor = choice $ map try
  [ (\txt -> Factor (AStr txt) TokenInfo) <$> litTextT
  , (\num -> Factor (ANum num) TokenInfo) <$> numberT
  , (\num -> Factor (ADecimal num) TokenInfo) <$> decimalT
  , (\reg -> Factor (ARegex reg) TokenInfo) <$> regexT
  , (\cmd -> Factor (AShellCommand cmd) TokenInfo) <$> shellCommandT
  , (\bool -> Factor (ABool bool) TokenInfo) <$> boolT
  , parseVector
  , parseDic
  , parensExp
  , parseIdentifier
  ]

parseVector :: TokenParser (Expression TokenInfo)
parseVector = do
  items <- between oBracketT cBracketT (parseExp `sepBy` commaT)
  return $ Factor (AVector items) TokenInfo

parseDic :: TokenParser (Expression TokenInfo)
parseDic = do
  let item = do
        key <- litTextT <|> nameIdT
        operatorT' ":"
        body <- parseExp
        return (key, body)
  items <- between oBraceT cBraceT (item `sepBy` commaT)
  return $ Factor (ADic items) TokenInfo
