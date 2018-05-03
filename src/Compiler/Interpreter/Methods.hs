{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Methods where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Data.Maybe
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import           Lens.Micro.Platform
import           System.Console.Haskeline

import           Compiler.Ast
import           Compiler.Instruction.Methods
import           Compiler.Instruction.Types
import           Compiler.Interpreter.Command.Methods
import           Compiler.Interpreter.Types
import           Compiler.Interpreter.Utils
import           Compiler.Parser.Methods
import           Compiler.Parser.Types
import           Compiler.Scope.Methods
import           Compiler.Token.Lexer                 (Tokenizer (..),
                                                       getTokens, scanner)
import           Compiler.World.Types
import           Text.Groom

-- | Initial State of interpreter
initialState :: IState
initialState = IState
  { _multiline = Nothing
  , _memory    = World {_table = mempty, _scope = initialScope}
  }

-- | Start an repl without prelude
--
-- TODO: Se tiene que declarar variables propias del interprete
repl :: Interpreter ()
repl = do
  inMultiline <- isJust <$> use multiline
  let prompt = if inMultiline then "... " else ">>> "
  minput <- lift . lift $ getInputLine prompt
  case minput of
    Nothing    -> return ()
    Just input -> do
      mText <- use multiline
      case mText of
        Just text
          | T.null . T.strip $ T.pack input -> do
            multiline .= Nothing
            compileSource text "**Interpreter**"
          | otherwise ->
            multiline .= Just (text `mappend` "\n" `mappend` T.pack input)
        Nothing -> do
          tokenizer' <- tokenizer $ T.pack input
          case tokenizer' of
            Partial  _ -> multiline .= Just (T.pack input)
            Complete _ -> compileSource (T.pack input) "**Interpreter**" `catchError` handleError
  repl

-- | Handle possible errors during execution of interpreter
handleError :: InterpreterError -> Interpreter ()
handleError err = case err of
  Compiling err' -> liftIO $ T.putStrLn err'
  Internal err'  -> liftIO $ T.putStrLn err'

-- | First phase of interpreter
tokenizer :: T.Text -> Interpreter Tokenizer
tokenizer input = case scanner False $ T.unpack input of
  Left err -> do
    liftIO $ putStrLn err
    return $ Complete mempty
  Right tokens -> return tokens

-- | Compile source code
-- TODO: Improve errors translation between them
compileSource :: T.Text -> String -> Interpreter ()
compileSource rawFile nameFile = do
  tokenizer' <- catchEither (Compiling . T.pack) . return . scanner True $ T.unpack rawFile
  ast <- catchEither (Compiling . T.pack . show) . return . parserLexer nameFile $ getTokens tokenizer'
  liftIO $ putStrLn $ groom ast
  case ast of
    Command cmd args -> executeCommand cmd args
    Code statements -> do
      expr      <- computeStatements statements
      astScoped <- catchEither (Compiling . T.pack . show) . liftScope $ scopingThroughtAST expr
      liftIO $ putStrLn $ groom  astScoped
      evaluateScopedProgram astScoped

-- | Evaluate program with AST already scoped
evaluateScopedProgram :: ExpressionG Identity TokenInfo AddressRef -> Interpreter ()
evaluateScopedProgram astScoped = do
  value <- liftWorld (runProgram (astToInstructions astScoped))
  case value of
    Just value' -> do
      showable <- showInterpreter value'
      liftIO $ putStrLn showable
    Nothing -> return ()

-- Computar las class y los import, unir todos los Expression con seq
computeStatements :: [Statement TokenInfo] -> Interpreter (Expression TokenInfo)
computeStatements =
  flip foldM (SeqExpr [] TokenInfo) $ \(SeqExpr exprs t) st ->
    case st of
      Import _path _ -> error "No implemented yet import functionality"
      cls@Class{}    -> do
        expr <- catchEither undefined . liftScope $ scopingClassAST cls
        evaluateScopedProgram expr
        return $ SeqExpr [] TokenInfo
      Expr expr _    -> return $ SeqExpr (expr : exprs) t
