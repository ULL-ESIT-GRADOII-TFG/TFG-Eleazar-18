module Compiler.Token.Lexer (alexScan) where

import qualified Compiler.Token.LexerGen as LexerGen
import           Compiler.Token.Types


alexScan :: AlexInput -> Int -> AlexReturn (AlexAction Lexeme)
alexScan = LexerGen.alexScan
