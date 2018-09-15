module Compiler.Token.Lexer where

import Compiler.Token.Types

alexScan :: AlexInput -> Int -> AlexReturn (AlexAction Lexeme)
