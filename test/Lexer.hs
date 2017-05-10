{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import Test.Hspec
import Text.Parsec

import GHS.Language.Lexer
import GHS.Language.Token

lexerTest :: SpecWith ()
lexerTest = do
  describe "Lexer Test" $ do
    it "It parse a simple number" $ do
      parse numberParser "TEST" "1234567890" `shouldBe` Right (NumT 1234567890)

    it "It parse a negative number" $ do
      parse numberParser "TEST" "-1" `shouldBe` Right (NumT (-1))

    it "It parse a positive number" $ do
      parse numberParser "TEST" "+0" `shouldBe` Right (NumT 0)

    it "It parse a literal string" $ do
      parse stringParser "TEST" "\"Into a string\"" `shouldBe` Right (LitTextT "Into a string")

  describe "Parse several Tokens" $ do
    it "In the same line" $ do
      parse parseTokens "TEST" "hello 4556 hi" `shouldBe` Right [NameIdT "hello", NumT 4556, NameIdT "hi"]

    it "Keywords" $ do
      parse parseTokens "TEST" "for as if else" `shouldBe` Right [ForT, AsT, IfT, ElseT]
