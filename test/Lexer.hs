{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import qualified Data.Vector          as V
import           Test.Hspec
-- import           Text.Parsec

import           Compiler.Token.Lexer
-- import           Compiler.Token.Methods
import           Compiler.Token.Types

tokenParse :: String -> Either String Token
tokenParse val = do
  tokenizer' <- scanner False val
  let tokens = getTokens tokenizer'
  if V.null tokens then
    Left "Empty scanner"
  else
    Right . tokn $ V.head tokens

tokenFlow :: String -> Either String [Token]
tokenFlow val = do
  tokenizer' <- scanner False val
  let tokens = getTokens tokenizer'
  Right $ V.toList $ fmap tokn tokens

lexerTest :: SpecWith ()
lexerTest =
  describe "Lexer Test" $ do
    it "It parse a variable names" $ do
      tokenParse "__test" `shouldBe` Right (NameIdT "__test")
      tokenParse "t1e2s3t4" `shouldBe` Right (NameIdT "t1e2s3t4")

    it "It parse a simple number" $
      tokenParse "1234567890" `shouldBe` Right (NumT 1234567890)

    it "It parse a decimal number" $
      tokenParse "1234567890.0123456789" `shouldBe` Right (DecimalT 1234567890.0123456789)

    it "It parse a bools token" $ do
      tokenParse "true" `shouldBe` Right (BoolT True)
      tokenParse "false" `shouldBe` Right (BoolT False)

    it "It parse token operators" $ do
      -- Current used operators or going to be used
      tokenParse "!" `shouldBe` Right (OperatorT "!")
      tokenParse "**" `shouldBe` Right (OperatorT "**")
      tokenParse "/" `shouldBe` Right (OperatorT "/")
      tokenParse "%" `shouldBe` Right (OperatorT "%")
      tokenParse "*" `shouldBe` Right (OperatorT "*")
      tokenParse "+" `shouldBe` Right (OperatorT "+")
      tokenParse "-" `shouldBe` Right (OperatorT "-")
      tokenParse "++" `shouldBe` Right (OperatorT "++")
      tokenParse "&&" `shouldBe` Right (OperatorT "&&")
      tokenParse "||" `shouldBe` Right (OperatorT "||")
      tokenParse "==" `shouldBe` Right (OperatorT "==")
      tokenParse ">" `shouldBe` Right (OperatorT ">")
      tokenParse "<" `shouldBe` Right (OperatorT "<")
      tokenParse ">=" `shouldBe` Right (OperatorT ">=")
      tokenParse "<=" `shouldBe` Right (OperatorT "<=")
      tokenParse "!=" `shouldBe` Right (OperatorT "!=")
      tokenParse "/=" `shouldBe` Right (OperatorT "/=")
      tokenParse "??" `shouldBe` Right (OperatorT "??")

    it "It parse a literal string" $ do
      tokenParse "\"Into a string\"" `shouldBe` Right (LitTextT "Into a string")
      tokenParse "\"Into \\nhere \na string\"" `shouldBe` Right (LitTextT "Into \nhere \na string")

    it "It parse a literal regex" $
      tokenParse "r\"Into a string\"" `shouldBe` Right (RegexExprT "Into a string")

    it "It parse a literal command" $
      tokenParse "$Into a string$" `shouldBe` Right (ShellCommandT "Into a string")

    it "It parse a literal command alternative sintax" $
      tokenParse "$\"Into a string\"" `shouldBe` Right (ShellCommandT "Into a string")

    it "Identation rules" $ do
      tokenFlow "test:\n  hi\n" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "hi",CBraceT]
      tokenFlow "test:\n  h1\n  h2\n" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "h1",NameIdT "h2",CBraceT]

      tokenFlow "test:\n  h1\n  h2\nh3" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "h1",NameIdT "h2",CBraceT, NameIdT "h3"]

      tokenFlow "test:\n  h1\n   \n\nh3" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "h1",CBraceT, NameIdT "h3"]

    it "TEST" $ do
      tokenFlow "for h in []:\n  hi\n" `shouldBe` Right [ForT,NameIdT "h", InT, OBracketT,CBracketT,OBraceT,NameIdT "hi",CBraceT]

    it "TEST2" $ do
      tokenFlow "for h in [] {}" `shouldBe` Right [ForT,NameIdT "h", InT, OBracketT,CBracketT,OBraceT,CBraceT]

    it "TEST3" $ do
      tokenFlow "if true {false} else {true}" `shouldBe` Right [IfT,BoolT True,OBraceT,BoolT False,CBraceT,ElseT,OBraceT,BoolT True,CBraceT]


  -- describe "Parse several Tokens" $ do
  --
  --   it "In the same line" $ do
  --     parse parseTokens "TEST" "hello 4556 hi" `shouldBe` Right [NameIdT "hello", NumT 4556, NameIdT "hi"]
  --
  --   it "Keywords" $ do
  --     parse parseTokens "TEST" "for as if else" `shouldBe` Right [ForT, AsT, IfT, ElseT]
