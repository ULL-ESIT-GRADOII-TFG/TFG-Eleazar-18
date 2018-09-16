{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Parser where

import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import           Development.IncludeFile
import           Test.Hspec
import           Text.Parsec

import           Compiler.Ast
import           Compiler.Parser.Methods
import           Compiler.Token.Methods

getAST :: BS.ByteString -> Either String Repl
getAST rawFile = do
  tokenizer' <- first show . scanner True $ BL.fromStrict rawFile
  first show . parserLexer "** TEST **" $ getTokens tokenizer'

$(includeFileInSource "./test/parser/01-vars.sflow" "vars01")

-- TODO: We need a Show instance of AST instance to Tok
--       This requires a lot manual work, with mantain
-- HUnit modify assertEqual to work with Pretty
-- parserTest :: SpecWith ()
-- parserTest = do
--   describe "Parser Simple" $ do
--     it "Code " $ do
--         getAST vars01 `shouldBe` Left ""
