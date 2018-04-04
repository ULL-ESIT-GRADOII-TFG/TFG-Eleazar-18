{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import Test.Hspec
import Text.Parsec
import qualified Data.Vector as V

import Compiler.Token.Types
import Compiler.Token.Lexer
import Compiler.Token.Methods

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
    it "It parse a simple number" $
      tokenParse "1234567890" `shouldBe` Right (NumT 1234567890)

    it "It parse a negative number" $
      tokenParse "-1" `shouldBe` Right (NumT (-1))

    it "It parse a positive number" $
      tokenParse "0" `shouldBe` Right (NumT 0)

    it "It parse a bools token" $ do
      tokenParse "true" `shouldBe` Right (BoolT True)
      tokenParse "false" `shouldBe` Right (BoolT False)

    it "It parse an operators token" $ do
      tokenParse "+" `shouldBe` Right (OperatorT "+")
      tokenParse "++" `shouldBe` Right (OperatorT "++")
      tokenParse "=&" `shouldBe` Right (OperatorT "=&")

    it "It parse a literal string" $
      tokenParse "\"Into a string\"" `shouldBe` Right (LitTextT "Into a string")

    it "It parse a literal string" $
      tokenParse "/Into a string/" `shouldBe` Right (RegexExprT "Into a string")

    it "It parse a literal string" $
      tokenParse "$Into a string$" `shouldBe` Right (ShellCommandT "Into a string")

    it "Identation rules" $ do
      tokenFlow "test:\n  hi\n" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "hi",CBraceT]
      tokenFlow "test:\n  h1\n  h2\n" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "h1",NameIdT "h2",CBraceT]

  -- describe "Parse several Tokens" $ do
  --   it "In the same line" $ do
  --     parse parseTokens "TEST" "hello 4556 hi" `shouldBe` Right [NameIdT "hello", NumT 4556, NameIdT "hi"]
  --
  --   it "Keywords" $ do
  --     parse parseTokens "TEST" "for as if else" `shouldBe` Right [ForT, AsT, IfT, ElseT]
