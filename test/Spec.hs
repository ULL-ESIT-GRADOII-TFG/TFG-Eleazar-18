module Main where

import Test.Hspec

import Lexer
import Parser
import Memory
import Programs


main :: IO ()
main = hspec $ do
  describe "ScripFlow Test" $ do
    lexerTest
    -- parserTest
    memoryTest
    programTest
