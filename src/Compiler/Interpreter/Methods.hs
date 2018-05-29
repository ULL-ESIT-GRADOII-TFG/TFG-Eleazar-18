{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Methods where

import           Control.Monad.Except
import           Data.Maybe
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import           Lens.Micro.Platform
import           System.Console.Haskeline
import           Text.Groom

import           Compiler.Ast
import           Compiler.Desugar.Types
import           Compiler.Instruction.Methods
import           Compiler.Interpreter.Command.Methods
import           Compiler.Interpreter.Utils
import           Compiler.Parser.Methods
import           Compiler.Scope.Methods  ()
import           Compiler.Scope.Utils
import           Compiler.Token.Lexer                 (Tokenizer (..), scanner)
import           Compiler.Types


-- | Start an repl without prelude
repl :: Interpreter ()
repl = do
  prompt' <- getPrompt
  minput  <- lift . lift $ getInputLine prompt'
  case minput of
    Nothing    -> return ()
    Just input -> do
      mText <- use multiline
      case mText of
        Just text
          | T.null . T.strip $ T.pack input -> do
            multiline .= Nothing
            compileSource text "**Interpreter**"
          | otherwise -> multiline
          .= Just (text `mappend` "\n" `mappend` T.pack input)
        Nothing -> do
          tokenizer' <- tokenizer $ T.pack input
          case tokenizer' of
            Partial _ -> multiline .= Just (T.pack input)
            Complete _ ->
              compileSource (T.pack input) "**Interpreter**"
                `catchError` handleError
  repl

-- | Get prompt from configuration try execute prompt code else show a
-- default prompt with error flag
getPrompt :: Interpreter String
getPrompt = do
  isMultiline <- isJust <$> use multiline
  flip catchError
    (\e -> do
      handleError e
      return $
        if isMultiline then "[ERROR] ... " else "[ERROR] >>> ")
    $ do
      mkPrompt    <-
        catchEither (Compiling . T.pack .show) $
          use (config . prompt . unPrompt) >>= liftScope . withNewScope
      value       <- catchMaybe (Internal "No value returned") . liftWorld $ runProgram mkPrompt
      case value of
        OStr text ->
          if isMultiline then return $ replicate (T.length text - 4) ' ' ++ "... " else return $ T.unpack text
        _ ->
          -- TODO: Maybe cast object to string object
          throwError $ Internal "Not a string value"

-- | Handle possible errors during execution of interpreter
-- TODO: Improve
handleError :: InterpreterError -> Interpreter ()
handleError err = case err of
  Compiling err' -> liftIO $ T.putStrLn err'
  Internal  err' -> liftIO $ T.putStrLn err'

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
  ast <- catchEither id . return $ generateAST rawFile nameFile
  verbosity <- use verboseLevel
  when (verbosity >= 2) $
    liftIO $ putStrLn $ groom ast
  case ast of
    Command cmd args -> executeCommand cmd args
    Code statements  -> do
      astScoped <- catchEither (Compiling . T.pack . show) . liftScope $
        computeStatements statements >>= transform
      when (verbosity >= 1) $
        liftIO $ putStrLn $ groom astScoped
      evaluateScopedProgram astScoped

compileSourcePure :: T.Text -> String -> Either InterpreterError (ScopeM Prog)
compileSourcePure rawFile nameFile = do
  ast <- generateAST rawFile nameFile
  case ast of
    Command _cmd _args -> Left $ Compiling "You can't use command"
    Code statements    -> Right $ do
      expr      <- computeStatements statements
      astScoped <- transform expr
      return $ astToInstructions astScoped

-- | Evaluate program with AST already scoped
evaluateScopedProgram :: Expression ScopeInfoAST -> Interpreter ()
evaluateScopedProgram astScoped = do
  value <- liftWorld (runProgram (astToInstructions astScoped))
  case value of
    Just value' -> do
      showable <- showInterpreter value'
      liftIO $ putStrLn showable
    Nothing -> return ()

-- Computar las class y los import, unir todos los Expression con seq
computeStatements :: [Statement TokenInfo] -> ScopeM (Expression TokenInfo)
computeStatements =
  flip foldM (SeqExpr [] dummyTokenInfo) $ \(SeqExpr exprs t) st -> case st of
      -- TODO
    Import _path _ -> error "No implemented yet. Import functionality"
    FunSt fun -> do
      _ <- transform fun
      return $ SeqExpr [] dummyTokenInfo
    ClassSt cls -> do
      _ <- transform cls
      return $ SeqExpr [] dummyTokenInfo
    Expr expr -> return $ SeqExpr (expr : exprs) t
