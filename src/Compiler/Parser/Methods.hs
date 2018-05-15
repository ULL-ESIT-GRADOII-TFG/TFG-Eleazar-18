{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Methods where

import           Data.Bifunctor
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Text.Parsec

import           Compiler.Ast
import           Compiler.Parser.Types
import           Compiler.Prelude.Methods (operatorsPrecedence)
import           Compiler.Prelude.Types   (Assoc (..))
import           Compiler.Token.Lexer     (Lexeme, getTokens, scanner)
import           Compiler.Token.Methods
import           Compiler.Types

{-
  TODO: Generate an useful `TokenInfo`
  TODO: Fail on use reserved words
 -}
generateAST :: T.Text -> String -> Either InterpreterError Repl
generateAST rawFile nameFile = do
  tokenizer' <- first (Compiling . T.pack) . scanner True $ T.unpack rawFile
  first (Compiling . T.pack . show) . parserLexer nameFile $ getTokens
    tokenizer'

parserLexer :: SourceName -> V.Vector Lexeme -> Either ParseError Repl
parserLexer = parse parseInterpreter

parseInterpreter :: TokenParser Repl
parseInterpreter =
  choice [uncurry Command <$> try iCommandT, Code <$> try parseStatements]
    <* eof

parseStatements :: TokenParser [Statement TokenInfo]
parseStatements =
  many
      ( choice $ map
        try
        [ parseClassStatement
        , parseImportStatement
        , (`Expr` TokenInfo) <$> parseSeqExpr
        ]
      )
    <* eof

parseImportStatement :: TokenParser (Statement TokenInfo)
parseImportStatement = do
  importT
  text <- litTextT
  return $ Import text TokenInfo

parseClassStatement :: TokenParser (Statement TokenInfo)
parseClassStatement = do
  classT
  nameCls <- classIdT
  bodyClass <- between oBraceT cBraceT parseBodyClass
  return $ Class nameCls bodyClass TokenInfo

parseBodyClass :: TokenParser (Expression TokenInfo)
parseBodyClass = choice $ map try [parseFunDecl, parseAssign, parseIdentifier]

parseExp :: TokenParser (Expression TokenInfo)
parseExp = choice $ map
  try
  [ parseUnaryOperators
  , parseFunDecl
  , parseLam
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
  return $ VarDecl
    (Simple funName TokenInfo)
    (FunDecl params prog TokenInfo)
    TokenInfo

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
  expr     <- parseExp
  return $ Apply (Simple operator TokenInfo) [expr] TokenInfo

parseApply :: TokenParser (Expression TokenInfo)
parseApply = do
  name   <- parseAccessor
  params <- between oParenT cParenT (parseSeqExpr `sepBy` commaT)
  return $ Apply name params TokenInfo

parseMethodChain :: TokenParser (Expression TokenInfo)
  -> TokenParser (Expression TokenInfo)
parseMethodChain parser = do
  expr <- parser
  expr' <- optionMaybe $ parseMethod expr
  maybe (return expr) (parseMethodChain . return) expr'

parseMethod :: Expression TokenInfo -> TokenParser (Expression TokenInfo)
parseMethod expr = do
  operatorT' "."
  acc    <- parseAccessor

  -- Apply
  params <- optionMaybe $ between
              oParenT
              cParenT
              (parseSeqExpr `sepBy` commaT)

  -- Braces
  param <- optionMaybe $ do
    between oBraceT cBraceT parseExp

  return $ MkScope
    [ VarDecl (Simple "aux" TokenInfo) expr TokenInfo
    , case (params, param) of
        (Just params', Just param') -> MkScope
          [ VarDecl
              (Simple "aux2" TokenInfo)
              (Apply (Dot "aux" acc TokenInfo) params' TokenInfo) TokenInfo
          , Apply
              (Dot "aux2" (Simple "__bracket__" TokenInfo) TokenInfo)
              [param'] TokenInfo
          ]

        (Just params', Nothing)     ->
          Apply (Dot "aux" acc TokenInfo) params' TokenInfo

        (Nothing, Just param')      -> MkScope
          [ VarDecl
              (Simple "aux2" TokenInfo)
              (Identifier (Dot "aux" acc TokenInfo) TokenInfo) TokenInfo
          , Apply
              (Dot "aux2" (Simple "__bracket__" TokenInfo) TokenInfo)
              [param'] TokenInfo
          ]

        (Nothing, Nothing)          -> Identifier (Dot "aux" acc TokenInfo) TokenInfo
    ]

checkOperator :: Int -> TokenParser T.Text
checkOperator level = do
  op <- operatorT
  case M.lookup op operatorsPrecedence of
    Just (levelOP, _assoc) -> if levelOP == level
      then return op
      else parserFail ("Operator in wrong level " ++ show level)
    Nothing -> parserFail "No valid operator"

levels :: [Int] -> TokenParser (Expression TokenInfo)
levels = foldl level (parseMethodChain parseFactor)
 where
  level nextLevel leveln = do
    expr      <- nextLevel
    listLevel <- many $ try ((,) <$> checkOperator leveln <*> nextLevel)
    return $ treeOperators expr listLevel

treeOperators
  :: Expression TokenInfo
  -> [(T.Text, Expression TokenInfo)]
  -> Expression TokenInfo
treeOperators expr []               = expr
treeOperators expr list@((op, _):_) = case M.lookup op operatorsPrecedence of
  Just (_, LeftAssoc) -> foldl
    (\acc (_, expr') -> Apply (Simple op TokenInfo) [acc, expr'] TokenInfo)
    expr
    list
  Just (_, RightAssoc) -> foldr1
    (\expr' acc -> Apply (Simple op TokenInfo) [expr', acc] TokenInfo)
    (expr : map snd list)
  Nothing -> error "Operator not found. Internal error"

parseOperators :: TokenParser (Expression TokenInfo)
parseOperators = levels [10, 9 .. 1]

parseAccessor :: TokenParser (Accessor TokenInfo)
parseAccessor = choice $ map
  try
  [ do
    name <- nameIdT
    operatorT' "."
    rest <- parseAccessor
    return $ Dot name rest TokenInfo
  , (`Simple` TokenInfo) <$> nameIdT
  ]

parseIdentifier :: TokenParser (Expression TokenInfo)
parseIdentifier = do
  name <- parseAccessor
  return $ Identifier name TokenInfo

parensExp :: TokenParser (Expression TokenInfo)
parensExp = between oParenT cParenT parseSeqExpr

parseFactor :: TokenParser (Expression TokenInfo)
parseFactor = choice $ map
  try
  [ (\txt -> Factor (AStr txt) TokenInfo) <$> litTextT
  , (\num -> Factor (ANum num) TokenInfo) <$> numberT
  , (\num -> Factor (ADecimal num) TokenInfo) <$> decimalT
  , (\reg -> Factor (ARegex reg) TokenInfo) <$> regexT
  , (\cmd -> Factor (AShellCommand cmd) TokenInfo) <$> shellCommandT
  , (\bool -> Factor (ABool bool) TokenInfo) <$> boolT
  , Factor ANone TokenInfo <$ noneT
  , parseVector
  , parseDic
  , parensExp
  , parseApply
  , parseIdentifier
  ]

parseVector :: TokenParser (Expression TokenInfo)
parseVector = do
  items <- between oBracketT cBracketT (parseExp `sepBy` commaT)
  return $ Factor (AVector items) TokenInfo

parseDic :: TokenParser (Expression TokenInfo)
parseDic = do
  let
    item = do
      key <- litTextT <|> nameIdT
      operatorT' ":"
      body <- parseExp
      return (key, body)
  items <- between oBraceT cBraceT (item `sepBy` commaT)
  return $ Factor (ADic items) TokenInfo
