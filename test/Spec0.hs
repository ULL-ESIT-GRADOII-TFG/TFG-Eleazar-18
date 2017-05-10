module Main where

import Test.Hspec

import Lexer
import Parser

main :: IO ()
main = hspec $ do
  describe "GitScript Test" $ do
    lexerTest
    parserTest

