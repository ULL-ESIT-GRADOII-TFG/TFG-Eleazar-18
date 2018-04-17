{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Methods where

import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Text                            as T
import           Lens.Micro.Platform
import           System.Console.Haskeline

import           Compiler.Ast
import           Compiler.Instruction.Methods
import           Compiler.Interpreter.Command.Methods
import           Compiler.Interpreter.Types
import           Compiler.Interpreter.Utils
import           Compiler.Parser.Methods
import           Compiler.Parser.Types
import           Compiler.Scope.Methods
import           Compiler.Token.Lexer                 (Tokenizer (..),
                                                       getTokens, scanner)
import           Compiler.World.Types


-- | Initial State of interpreter
initialState :: IState
initialState = IState
  { _multiline = Nothing
  , _memory    = World {_table = mempty, _typeDefinitions = mempty, _scope = initialScope}
  }

-- | Start an repl without prelude
--
-- TODO: Se tiene que declarar variables propias del interprete
repl :: Interpreter ()
repl = do
  inMultiline <- isJust <$> use multiline
  let prompt = if inMultiline then "... " else ">>> "
  minput <- lift $ getInputLine prompt
  case minput of
    Nothing    -> return ()
    Just input -> do
      mText <- use multiline
      case mText of
        Just text
          | T.null . T.strip $ T.pack input -> do
            multiline .= Nothing
            compileFile text "**Interpreter**"
          | otherwise ->
            multiline .= Just (text `mappend` "\n" `mappend` T.pack input)
        Nothing -> do
          tokenizer' <- tokenizer $ T.pack input
          case tokenizer' of
            Partial  _ -> multiline .= Just (T.pack input)
            Complete _ -> compileFile (T.pack input) "**Interpreter**"
  repl

-- | First phase of interpreter
tokenizer :: T.Text -> Interpreter Tokenizer
tokenizer input = case scanner False $ T.unpack input of
  Left err -> do
    liftIO $ putStrLn err
    return $ Complete mempty
  Right tokens -> return tokens

-- | Compile a file
compileFile :: T.Text -> String -> Interpreter ()
compileFile rawFile nameFile = do
  let ast = do
        tokenizer' <- scanner True $ T.unpack rawFile
        first show (parserLexer nameFile (getTokens tokenizer')) -- TODO Take better Error
  case ast of
    Left  err  -> liftIO $ putStrLn err
    Right ast' -> do
      liftIO $ print ast'
      mStatements <- tryExecuteICommand ast'
      case mStatements of
        Nothing         -> return ()
        Just statements -> do
          expr      <- computeStatements statements
          astScoped <- liftScope $ scopingThroughtAST expr
          case astScoped of
            Right scopeAst -> do
              value <- liftWorld (runProgram (astToInstructions scopeAst))
              case value of
                Just value' -> do
                  showable <- showInterpreter value'
                  liftIO $ putStrLn showable
                Nothing -> return ()
            Left err -> liftIO $ print err

tryExecuteICommand :: Repl -> Interpreter (Maybe [Statement TokenInfo])
tryExecuteICommand (Command cmd args) =
  executeCommand cmd args >> return Nothing
tryExecuteICommand (Code statements) = return $ Just statements

-- Computar las class y los import, unir todos los Expression con seq
computeStatements :: [Statement TokenInfo] -> Interpreter (Expression TokenInfo)
computeStatements =
  flip foldM (SeqExpr [] TokenInfo) $ \(SeqExpr exprs t) st ->
    case st of
      Import _path _ -> error "No implemented yet import functionality"
      cls@Class{}    -> do
        _ <- liftScope $ scopingClassAST cls
        return $ SeqExpr [] TokenInfo
      Expr expr _    -> return $ SeqExpr (expr : exprs) t