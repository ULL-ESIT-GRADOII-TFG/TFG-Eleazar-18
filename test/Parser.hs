{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Test.Hspec
import Text.Parsec
--
-- import GHS.Language.Lexer
-- import GHS.Language.Token
-- import GHS.Language.Parser
-- import GHS.Language.AST
--
-- parserTest :: SpecWith ()
-- parserTest = do
--   describe "Parser Simple" $ do
--     it "Code " $ do
--       parse parseLanguage "TEST" [NameIdT "hello", OParenT, NameIdT "arg", CParenT] `shouldBe` Right [Expr (Apply () "hello" [Apply () "arg" []])]
--
--     it "Other" $ do
--       parse parseStatements "TEST" [NameIdT "hello"] `shouldBe` Right (Expr (Apply () "hello" []))
--
--     it "other2" $ do
--       parse parseExp "TEST" [NameIdT "hello"] `shouldBe` Right (Apply () "hello" [])
--
--     it "other4" $ do
--       parse parseApply "TEST" [NameIdT "hello"] `shouldBe` Right (Apply () "hello" [])
--
--   describe "Parse Statemenmts" $ do
--     it "IF statement" $ do
--       parse parseStatements "TEST" [NameIdT "var1", AssignT, LitTextT "hello"] `shouldBe` Right [Assign () "var1" [Expr (LitText "hello")]]
