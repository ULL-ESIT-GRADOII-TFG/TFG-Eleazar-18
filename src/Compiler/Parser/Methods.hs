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
  TODO: Fail on use reserved words
 -}

mkTokenInfo :: TokenParser (TokenInfo -> a) -> TokenParser a
mkTokenInfo parser = do
  initialPos <- getPosition
  func <- parser
  finalPos <- getPosition
  return (func (TokenInfo (toSrcPos initialPos) (toSrcPos finalPos)))
  where
    toSrcPos :: SourcePos -> SrcPos
    toSrcPos sourcePos = SrcPos (sourceLine sourcePos) (sourceColumn sourcePos) 0

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
        [ ClassSt <$> parseClassStatement
        , parseImportStatement
        , Expr <$> parseSeqExpr
        ]
      )
    <* eof

parseImportStatement :: TokenParser (Statement TokenInfo)
parseImportStatement = mkTokenInfo $ do
  importT
  text <- litTextT
  return $ Import text

parseClassStatement :: TokenParser (ClassDecl TokenInfo)
parseClassStatement = mkTokenInfo $ do
  classT
  nameCls <- classIdT
  methods <- between oBraceT cBraceT parseMethodsClass
  return $ ClassDecl nameCls methods

parseMethodsClass :: TokenParser [FunDecl TokenInfo]
parseMethodsClass = many parseFunDecl

parseExp :: TokenParser (Expression TokenInfo)
parseExp = choice $ map
  try
  [ parseUnaryOperators
  -- , parseFunDecl
  , parseLam
  , parseIfElse
  , parseIf
  , parseFor
  , parseAssign
  , parseOperators
  ]

parseSeqExpr :: TokenParser (Expression TokenInfo)
parseSeqExpr = mkTokenInfo $ SeqExpr <$> many1 parseExp

parseBody :: TokenParser (Expression TokenInfo)
parseBody = try (mkTokenInfo $ do
  oBraceT >> cBraceT
  return $ SeqExpr []) <|> between oBraceT cBraceT parseSeqExpr

parseFunDecl :: TokenParser (FunDecl TokenInfo)
parseFunDecl = mkTokenInfo $ do
  funT
  funName <- nameIdT
  params  <- many nameIdT
  prog    <- parseBody
  return $ \info -> FunDecl
    funName
    params
    prog
    info

parseLam :: TokenParser (Expression TokenInfo)
parseLam = mkTokenInfo $ do
  lamT
  params <- many nameIdT
  prog   <- parseBody
  return $ FunExpr params prog

parseAssign :: TokenParser (Expression TokenInfo)
parseAssign = mkTokenInfo $ do
  varName <- parseAccessor
  assignT
  expr <- parseExp
  return $ VarExpr varName expr

parseIf :: TokenParser (Expression TokenInfo)
parseIf = mkTokenInfo $ do
  ifT
  expr <- parseExp
  prog <- parseBody
  return $ If expr prog

parseIfElse :: TokenParser (Expression TokenInfo)
parseIfElse = mkTokenInfo $ do
  ifT
  expr     <- parseExp
  progTrue <- parseBody
  elseT
  progFalse <- parseBody
  return $ IfElse expr progTrue progFalse

parseFor :: TokenParser (Expression TokenInfo)
parseFor = mkTokenInfo $ do
  forT
  nameVar <- nameIdT
  inT
  expr <- parseExp
  prog    <- parseBody
  return $ For nameVar expr prog

parseUnaryOperators :: TokenParser (Expression TokenInfo)
parseUnaryOperators = mkTokenInfo $ do
  operator <- mkTokenInfo $ Simple <$> operatorT
  expr     <- parseExp
  return $ Apply operator [expr]

parseApply :: TokenParser (Expression TokenInfo)
parseApply = mkTokenInfo $ do
  name   <- parseAccessor
  params <- between oParenT cParenT (parseSeqExpr `sepBy` commaT)
  return $ Apply name params

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
  param <- optionMaybe $
    between oBraceT cBraceT parseExp

  return $ MkScope
    [ VarExpr (Simple "aux" dummyTokenInfo) expr dummyTokenInfo
    , case (params, param) of
        (Just params', Just param') -> MkScope
          [ VarExpr
              (Simple "aux2" dummyTokenInfo)
              (Apply (Dot "aux" acc dummyTokenInfo) params' dummyTokenInfo) dummyTokenInfo
          , Apply
              (Dot "aux2" (Simple "__bracket__" dummyTokenInfo) dummyTokenInfo)
              [param'] dummyTokenInfo
          ]

        (Just params', Nothing)     ->
          Apply (Dot "aux" acc dummyTokenInfo) params' dummyTokenInfo

        (Nothing, Just param')      -> MkScope
          [ VarExpr
              (Simple "aux2" dummyTokenInfo)
              (Identifier (Dot "aux" acc dummyTokenInfo) dummyTokenInfo) dummyTokenInfo
          , Apply
              (Dot "aux2" (Simple "__bracket__" dummyTokenInfo) dummyTokenInfo)
              [param'] dummyTokenInfo

          ]

        (Nothing, Nothing)          -> Identifier (Dot "aux" acc dummyTokenInfo) dummyTokenInfo


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
    (\acc (_, expr') -> Apply (Simple op dummyTokenInfo) [acc, expr'] dummyTokenInfo)
    expr
    list
  Just (_, RightAssoc) -> foldr1
    (\expr' acc -> Apply (Simple op dummyTokenInfo) [expr', acc] dummyTokenInfo)
    (expr : map snd list)
  Nothing -> error "Operator not found. Internal error"

parseOperators :: TokenParser (Expression TokenInfo)
parseOperators = levels [10, 9 .. 1]

parseAccessor :: TokenParser (Accessor TokenInfo)
parseAccessor = choice $ map
  try
  [ mkTokenInfo $ do
    name <- nameIdT
    operatorT' "."
    rest <- parseAccessor
    return $ Dot name rest
  , mkTokenInfo $ Simple <$> classIdT -- Class constructor
  , mkTokenInfo $ Simple <$> nameIdT
  ]

parseIdentifier :: TokenParser (Expression TokenInfo)
parseIdentifier = mkTokenInfo $ Identifier <$> parseAccessor

parensExp :: TokenParser (Expression TokenInfo)
parensExp = between oParenT cParenT parseSeqExpr

parseFactor :: TokenParser (Expression TokenInfo)
parseFactor = choice $ map
  try
  [ mkTokenInfo $ Factor . AStr <$> litTextT
  , mkTokenInfo $ Factor . ANum <$> numberT
  , mkTokenInfo $ Factor . ADecimal <$> decimalT
  , mkTokenInfo $ Factor . ARegex <$> regexT
  , mkTokenInfo $ Factor . AShellCommand <$> shellCommandT
  , mkTokenInfo $ Factor . ABool <$> boolT
  , mkTokenInfo $ Factor ANone <$ noneT
  , parseVector
  , parseDic
  , parensExp
  , parseApply
  , parseIdentifier
  ]

parseVector :: TokenParser (Expression TokenInfo)
parseVector = mkTokenInfo $ Factor . AVector <$>
     between oBracketT cBracketT (parseExp `sepBy` commaT)

parseDic :: TokenParser (Expression TokenInfo)
parseDic = mkTokenInfo $ do
  let
    item = do
      key <- litTextT <|> nameIdT
      operatorT' ":"
      body <- parseExp
      return (key, body)
  items <- between oBraceT cBraceT (item `sepBy` commaT)
  return $ Factor (ADic items)
