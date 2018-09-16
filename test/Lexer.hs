{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lexer where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as V
import           Test.Hspec
import           Development.IncludeFile

import           Compiler.Token.Methods
import           Compiler.Token.Types

$(includeFileInSource "./test/tokenizer/01-braces.sflow" "braces01")
$(includeFileInSource "./test/tokenizer/01-ident.sflow" "ident01")

$(includeFileInSource "./test/tokenizer/02-braces.sflow" "braces02")
$(includeFileInSource "./test/tokenizer/02-ident.sflow" "ident02")

tokenParse :: BL.ByteString -> Either ErrorTokenizer Token
tokenParse val = do
  tokenizer' <- scanner False val
  let tokens = getTokens tokenizer'
  if V.null tokens then
    Left $ ErrorTokenizer "Empty scanner"
  else
    Right . tokn $ V.head tokens

tokenFlow :: BL.ByteString -> Either ErrorTokenizer [Token]
tokenFlow val = do
  tokenizer' <- scanner False val
  let tokens = getTokens tokenizer'
  Right $ V.toList $ fmap tokn tokens

tokenFlow' :: BL.ByteString -> Either ErrorTokenizer [Token]
tokenFlow' val = do
  tokenizer' <- scanner True val
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
      tokenParse "r/Into a string/" `shouldBe` Right (RegexExprT "Into a string")

    it "It parse a literal command one line" $ do
      tokenParse "$Into a string\n" `shouldBe` Right (ShellCommandT "Into a string")
      tokenParse "$Into a string" `shouldBe` Right (ShellCommandT "Into a string")

    it "It parse a literal command alternative sintax" $
      tokenParse "$\"Into a string\"" `shouldBe` Right (ShellCommandT "Into a string")

    it "Identation rules" $ do
      tokenFlow "test:\n  hi\n" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "hi",CBraceT]
      tokenFlow "test:\n  h1\n  h2\n" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "h1", EndStmtT, NameIdT "h2",CBraceT]

      tokenFlow "test:\n  h1\n  h2\nh3" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "h1", EndStmtT, NameIdT "h2",CBraceT, NameIdT "h3"]

      tokenFlow "test:\n  h1\n   \n\nh3" `shouldBe` Right [NameIdT "test",OBraceT,NameIdT "h1",CBraceT, NameIdT "h3"]

    it "Parses dic items" $
      tokenFlow "{a -> 5}" `shouldBe` Right [OBraceT,NameIdT "a",OperatorT "->",NumT 5,CBraceT]

    it "Parses __brace__ items" $
      tokenFlow "list[1]" `shouldBe` Right [NameIdT "list", OBracketT, NumT 1, CBracketT]

    it "TEST" $
      tokenFlow "for h in []:\n  hi\n" `shouldBe` Right [ForT,NameIdT "h", InT, OBracketT,CBracketT,OBraceT,NameIdT "hi",CBraceT]

    it "TEST2" $
      tokenFlow "for h in [] {}" `shouldBe` Right [ForT,NameIdT "h", InT, OBracketT,CBracketT,OBraceT,CBraceT]

    it "TEST3" $
      tokenFlow "if true {false} else {true}" `shouldBe` Right [IfT,BoolT True,OBraceT,BoolT False,CBraceT,ElseT,OBraceT,BoolT True,CBraceT]

    it "Indentation" $ do
      let ex1 = "class Test:\n\
                \  fun name:\n\
                \    test\n"
      tokenFlow ex1 `shouldBe`
         Right [ClassT, ClassIdT "Test", OBraceT, FunT, NameIdT "name", OBraceT, NameIdT "test", CBraceT, CBraceT]


      let ex2 = "class Test:\n\
                \  fun name:\n\
                \    test\n\
                \  fun other:\n\
                \    test2\n"
      tokenFlow ex2 `shouldBe`
         Right [ClassT, ClassIdT "Test", OBraceT, FunT, NameIdT "name", OBraceT, NameIdT "test", CBraceT, FunT, NameIdT "other", OBraceT, NameIdT "test2", CBraceT, CBraceT]

    it "Indentation - Complex with comments - NOTE: Dont take care EndStmtT" $ do
      -- Note this files must match in CBRACE and OBRACE, it is possible they
      -- unmatch because of EndStmts Tokens (They have a different behavior in
      -- each syntax)
      tokenFlow' (BL.fromStrict braces02) `shouldBe` tokenFlow' (BL.fromStrict ident02)
      tokenFlow' (BL.fromStrict braces01) `shouldBe` tokenFlow' (BL.fromStrict ident01)