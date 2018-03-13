module Compiler.Interpreter.Methods where

import qualified Data.Text as T
import qualified Text.Parsec as P
import           Control.Monad.State.Strict

import Compiler.Instruction.Methods
import Compiler.Scope.Methods
import Compiler.Parser.Methods
import Compiler.Token.Methods


compileFile :: T.Text -> P.SourceName -> IO ()
compileFile rawFile name =
  case P.parse parseTokens name rawFile of
    Left _ -> return ()
    Right tokens ->
      case P.parse parseExp name tokens of
        Left _ -> return ()
        Right ast -> do
          let scopeAst = evalState (scopingThroughtAST ast) undefined
          _ <- runStateT (runProgram (astToInstructions scopeAst)) undefined
          return ()
