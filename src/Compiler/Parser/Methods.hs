{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Methods where

import           Control.Monad
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


-- | List of reserved keywords
keywords :: [T.Text]
keywords =
  ["true", "false", "import", "lam",
   "in", "class", "fun", "for", "if",
   "else", "none"]

mkTokenInfo :: TokenParser (TokenInfo -> a) -> TokenParser a
mkTokenInfo parser = do
  initialPos <- getPosition
  func <- parser
  finalPos <- getPosition
  return (func (TokenInfo (toSrcPos initialPos) (toSrcPos finalPos)))

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
        , FunSt <$> parseFunDecl
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

parseMkScope :: TokenParser (Expression TokenInfo)
parseMkScope = mkTokenInfo $ MkScope <$> many1 parseExp

parseBody :: TokenParser (Expression TokenInfo)
parseBody = try (mkTokenInfo $ do
  oBraceT >> cBraceT
  return $ MkScope []) <|> between oBraceT cBraceT parseMkScope

parseFunDecl :: TokenParser (FunDecl TokenInfo)
parseFunDecl = mkTokenInfo $ do
  funT
  funcName <- nameIdT
  params   <- many nameIdT
  prog     <- parseBody
  return $ \info -> FunDecl
    funcName
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
  params <- between oParenT cParenT (parseMkScope `sepBy` commaT)
  return $ Apply name params

parseMethodChain :: TokenParser (Expression TokenInfo)
  -> TokenParser (Expression TokenInfo)
parseMethodChain parser = do
  expr <- parser
  expr' <- optionMaybe $ parseMethod expr
  maybe (return expr) (parseMethodChain . return) expr'

parseMethod :: Expression TokenInfo -> TokenParser (Expression TokenInfo)
parseMethod expr = do
  initialPos <- getPosition
  operatorT' "."
  acc    <- parseAccessor

  -- Apply
  params <- optionMaybe $ between
              oParenT
              cParenT
              (parseMkScope `sepBy` commaT)

  -- Braces
  param <- optionMaybe $
    between oBraceT cBraceT parseExp

  finalPos <- getPosition
  let tok = TokenInfo (toSrcPos initialPos) (toSrcPos finalPos)
  return $ MkScope
    [ VarExpr (Simple "parser_aux_0" tok) expr tok
    , case (params, param) of
        (Just params', Just param') -> MkScope
          [ VarExpr
              (Simple "parser_aux_2" tok)
              (Apply (Dot "parser_aux_0" acc tok) params' tok) tok
          , Apply
              (Dot "parser_aux_2" (Simple "__bracket__" tok) tok)
              [param'] tok
          ] tok

        (Just params', Nothing)     ->
          Apply (Dot "parser_aux_0" acc tok) params' tok

        (Nothing, Just param')      -> MkScope
          [ VarExpr
              (Simple "parser_aux_2" tok)
              (Identifier (Dot "parser_aux_0" acc tok) tok) tok
          , Apply
              (Dot "parser_aux_2" (Simple "__bracket__" tok) tok)
              [param'] tok

          ] tok

        (Nothing, Nothing)          -> Identifier (Dot "parser_aux_0" acc tok) tok
    ] tok

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

-- TODO: Add right TokenInfo
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
    when (name `elem` keywords) $ unexpected "Keyword in a name variable"
    operatorT' "."
    rest <- parseAccessor
    return $ Dot name rest
  , mkTokenInfo $ Simple <$> classIdT -- Class constructor
  , mkTokenInfo $ Simple <$> nameIdT
  ]

parseIdentifier :: TokenParser (Expression TokenInfo)
parseIdentifier = mkTokenInfo $ Identifier <$> parseAccessor

parensExp :: TokenParser (Expression TokenInfo)
parensExp = between oParenT cParenT parseExp

parseFactor :: TokenParser (Expression TokenInfo)
parseFactor = choice $ map
  try
  [ mkTokenInfo $ (\v i -> Factor (AStr v i) i) <$> litTextT
  , mkTokenInfo $ (\v i -> Factor (ANum v i) i)<$> numberT
  , mkTokenInfo $ (\v i -> Factor (ADecimal v i) i)<$> decimalT
  , mkTokenInfo $ (\v i -> Factor (ARegex v i) i)<$> regexT
  , mkTokenInfo $ (\v i -> Factor (AShellCommand v i) i)<$> shellCommandT
  , mkTokenInfo $ (\v i -> Factor (ABool v i) i)<$> boolT
  , mkTokenInfo $ (\i -> Factor (ANone i) i) <$ noneT
  , parseVector
  , parseDic
  , parensExp
  , parseApply
  , parseIdentifier
  ]

parseVector :: TokenParser (Expression TokenInfo)
parseVector = mkTokenInfo $ (\v i -> Factor (AVector v i) i)<$>
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
  return $ \info -> Factor (ADic items info) info
