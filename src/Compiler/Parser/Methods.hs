{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Methods where

import           Control.Monad
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Text.Parsec

import           Compiler.Ast
import           Compiler.Parser.Types
import           Compiler.Prelude.Utils (Assoc (..), operatorsPrecedence)
import           Compiler.Token.Lexer   (Lexeme)
import           Compiler.Token.Methods


-- | List of reserved keywords
keywords :: [T.Text]
keywords =
  ["true", "false", "import", "lam",
   "in", "class", "fun", "for", "if",
   "else", "none"]

-- | Helper to generate location info
mkTokenInfo :: TokenParser (TokenInfo -> a) -> TokenParser a
mkTokenInfo parser = do
  initialPos <- getPosition
  func <- parser
  finalPos <- getPosition
  return (func (TokenInfo (toSrcPos initialPos) (toSrcPos finalPos)))

toSrcPos :: SourcePos -> SrcPos
toSrcPos sourcePos = SrcPos (sourceLine sourcePos) (sourceColumn sourcePos) 0

parserLexer :: SourceName -> V.Vector Lexeme -> Either ParseError Repl
parserLexer = parse parseInterpreter

parseInterpreter :: TokenParser Repl
parseInterpreter =
  choice [uncurry Command <$> try iCommandT, Code <$> try parseStatements]
    <* eof

parseStatements :: TokenParser [Statement Tok]
parseStatements =
  many
      ( choice $ map
        try
        [ ClassSt <$> parseClassStatement
        , FunSt <$> parseFunDecl
        , Expr <$> parseSeqExpr
        ]
      )
    <* eof

parseClassStatement :: TokenParser (ClassDecl Tok)
parseClassStatement = mkTokenInfo $ do
  classT
  nameCls <- classIdT
  methods <- between oBraceT cBraceT parseMethodsClass
  return $ ClassDecl nameCls methods

parseMethodsClass :: TokenParser [FunDecl Tok]
parseMethodsClass = many parseFunDecl

parseExp :: TokenParser (Expression Tok)
parseExp = choice $ map
  try
  [ parseUnaryOperators
  , parseLam
  , parseIfElse
  , parseIf
  , parseFor
  , parseAssign
  , parseOperators
  ]

parseSeqExpr :: TokenParser (Expression Tok)
parseSeqExpr =
  mkTokenInfo $ TokSeqExpr <$> many1 (do
                                      expr <- parseExp
                                      void $ many endStmtT
                                      return expr
                                  )

parseMkScope :: TokenParser (Expression Tok)
parseMkScope =
  mkTokenInfo $ TokMkScope <$> many1 (do
                                      expr <- parseExp
                                      void $ many endStmtT
                                      return expr
                                  )

parseBody :: TokenParser (Expression Tok)
parseBody = try (mkTokenInfo $ do
  oBraceT >> cBraceT
  return $ TokMkScope []) <|> between oBraceT cBraceT parseMkScope

parseFunDecl :: TokenParser (FunDecl Tok)
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

parseLam :: TokenParser (Expression Tok)
parseLam = mkTokenInfo $ do
  lamT
  params <- many nameIdT
  prog   <- parseBody
  return $ TokFunExpr params prog

parseAssign :: TokenParser (Expression Tok)
parseAssign = mkTokenInfo $ do
  varName <- parseAccessor
  assignT
  expr <- parseExp
  return $ TokVarExpr varName expr

parseIf :: TokenParser (Expression Tok)
parseIf = mkTokenInfo $ do
  ifT
  expr <- parseExp
  prog <- parseBody
  return $ TokIf expr prog

parseIfElse :: TokenParser (Expression Tok)
parseIfElse = mkTokenInfo $ do
  ifT
  expr     <- parseExp
  progTrue <- parseBody
  elseT
  progFalse <- parseBody
  return $ TokIfElse expr progTrue progFalse

parseFor :: TokenParser (Expression Tok)
parseFor = mkTokenInfo $ do
  forT
  nameVar <- nameIdT
  inT
  expr <- parseExp
  prog <- parseBody
  return $ TokFor nameVar expr prog

parseUnaryOperators :: TokenParser (Expression Tok)
parseUnaryOperators = mkTokenInfo $ do
  operator <- mkTokenInfo $ Simple <$> operatorT
  expr     <- parseExp
  return $ TokApply operator [expr]

parseApply :: TokenParser (Expression Tok)
parseApply = mkTokenInfo $ do
  name   <- parseAccessor
  params <- between oParenT cParenT (parseExp `sepBy` commaT) <|> many1 parseFactorNoApply
  return $ TokApply name params

parseMethodChain :: TokenParser (Expression Tok)
  -> TokenParser (Expression Tok)
parseMethodChain parser = do
  expr <- parser
  let parseMethod' e = do
        expr' <- optionMaybe $ parseMethod e
        maybe (return e) parseMethod' expr'
  parseMethod' expr

parseMethod :: Expression Tok -> TokenParser (Expression Tok)
parseMethod expr = do
  initialPos <- getPosition
  nextExpr <- choice
    [ do
      operatorT' "."
      acc <- parseAccessor

     -- Apply
      params <- optionMaybe $ between
                  oParenT
                  cParenT
                  (parseExp `sepBy` commaT) <|> many1 parseFactorNoApply

      -- Braces
      param <- optionMaybe $ between oBracketT cBracketT parseExp

      return $ \tok ->
        case (params, param) of
            (Just params', Just param') -> TokMkScope
              [ TokVarExpr
                  (Simple "parser_aux_2" tok)
                  (TokApply (Dot "parser_aux_0" acc tok) params' tok) tok
              , TokApply
                  (Dot "parser_aux_2" (Simple "__at__" tok) tok)
                  [param'] tok
              ] tok

            (Just params', Nothing)     ->
              TokApply (Dot "parser_aux_0" acc tok) params' tok

            (Nothing, Just param')      -> TokMkScope
              [ TokVarExpr
                  (Simple "parser_aux_2" tok)
                  (TokIdentifier (Dot "parser_aux_0" acc tok) tok) tok
              , TokApply
                  (Dot "parser_aux_2" (Simple "__at__" tok) tok)
                  [param'] tok

              ] tok

            (Nothing, Nothing)          -> TokIdentifier (Dot "parser_aux_0" acc tok) tok

    , do
      -- Apply
      params <- between
                  oParenT
                  cParenT
                  (parseExp `sepBy` commaT)
      return $ \tok -> TokApply (Simple "parser_aux_0" tok) params tok
    , do
      -- At
      param <- between oBracketT cBracketT parseExp
      return $ \tok -> TokApply (Dot "parser_aux_0" (Simple "__at__" tok) tok) [param] tok
    ]

  finalPos <- getPosition

  let tok = TokenInfo (toSrcPos initialPos) (toSrcPos finalPos)
  return $ TokMkScope
    [ TokVarExpr (Simple "parser_aux_0" tok) expr tok
    , nextExpr tok
    ] tok

checkOperator :: Int -> TokenParser T.Text
checkOperator level = do
  op <- operatorT
  case HM.lookup op operatorsPrecedence of
    Just (levelOP, _assoc, _special) -> if levelOP == level
      then return op
      else parserFail ("Operator in wrong level " ++ show level)
    Nothing -> parserFail "No valid operator"

levels :: [Int] -> TokenParser (Expression Tok)
levels = foldl level (parseMethodChain parseFactor)
 where
  level nextLevel leveln = do
    expr      <- nextLevel
    listLevel <- many $ try ((,) <$> checkOperator leveln <*> nextLevel)
    return $ treeOperators expr listLevel

-- TODO: Add right Tok
treeOperators
  :: Expression Tok
  -> [(T.Text, Expression Tok)]
  -> Expression Tok
treeOperators expr []               = expr
treeOperators expr list@((op, _):_) = case HM.lookup op operatorsPrecedence of
  Just (_, LeftAssoc, _) -> foldl
    (\acc (_, expr') -> TokApply (Simple op dummyTokenInfo) [acc, expr'] dummyTokenInfo)
    expr
    list
  Just (_, RightAssoc, _) -> foldr1
    (\expr' acc -> TokApply (Simple op dummyTokenInfo) [expr', acc] dummyTokenInfo)
    (expr : map snd list)
  Nothing -> error "Operator not found. Internal error"

parseOperators :: TokenParser (Expression Tok)
parseOperators = levels [10, 9 .. 1]

parseAccessor :: TokenParser (Accessor Tok)
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

parseIdentifier :: TokenParser (Expression Tok)
parseIdentifier = mkTokenInfo $ TokIdentifier <$> parseAccessor

parensExp :: TokenParser (Expression Tok)
parensExp = between oParenT cParenT parseExp

parseFactor :: TokenParser (Expression Tok)
parseFactor = choice $ map
  try
  [ mkTokenInfo $ (\v i -> TokFactor (AStr v i) i) <$> litTextT
  , mkTokenInfo $ (\v i -> TokFactor (ANum v i) i)<$> numberT
  , mkTokenInfo $ (\v i -> TokFactor (ADecimal v i) i)<$> decimalT
  , mkTokenInfo $ (\v i -> TokFactor (ARegex v i) i)<$> regexT
  , mkTokenInfo $ (\v i -> TokFactor (AShellCommand v i) i)<$> shellCommandT
  , mkTokenInfo $ (\v i -> TokFactor (ABool v i) i)<$> boolT
  , mkTokenInfo $ (\i -> TokFactor (ANone i) i) <$ noneT
  , parseVector
  , parseDic
  , parensExp
  , parseApply
  , parseIdentifier
  ]

parseFactorNoApply :: TokenParser (Expression Tok)
parseFactorNoApply = choice $ map
  try
  [ mkTokenInfo $ (\v i -> TokFactor (AStr v i) i) <$> litTextT
  , mkTokenInfo $ (\v i -> TokFactor (ANum v i) i)<$> numberT
  , mkTokenInfo $ (\v i -> TokFactor (ADecimal v i) i)<$> decimalT
  , mkTokenInfo $ (\v i -> TokFactor (ARegex v i) i)<$> regexT
  , mkTokenInfo $ (\v i -> TokFactor (AShellCommand v i) i)<$> shellCommandT
  , mkTokenInfo $ (\v i -> TokFactor (ABool v i) i)<$> boolT
  , mkTokenInfo $ (\i -> TokFactor (ANone i) i) <$ noneT
  , parseVector
  , parseDic
  , parensExp
  , parseIdentifier
  ]

parseVector :: TokenParser (Expression Tok)
parseVector = mkTokenInfo $ (\v i -> TokFactor (AVector v i) i)<$>
     between oBracketT cBracketT (parseExp `sepBy` commaT)

parseDic :: TokenParser (Expression Tok)
parseDic = mkTokenInfo $ do
  let
    item = do
      key <- litTextT <|> nameIdT
      operatorT' "->"
      body <- parseExp
      return (key, body)
  items <- between oBraceT cBraceT (item `sepBy` commaT)
  return $ \info -> TokFactor (ADic items info) info
