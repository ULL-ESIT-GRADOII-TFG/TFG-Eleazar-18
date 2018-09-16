module Main where

import Test.Hspec

import Lexer
import Parser
import Memory

main :: IO ()
main = hspec $ do
  describe "ScripFlow Test" $ do
    lexerTest
    memoryTest
    --parserTest
