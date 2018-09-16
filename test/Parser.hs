{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Monad
import Data.Bifunctor
import Test.Hspec
import Text.Parsec
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString         as BS
import           Development.IncludeFile

import Compiler.Ast
import Compiler.Token.Methods
import Compiler.Parser.Methods

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
