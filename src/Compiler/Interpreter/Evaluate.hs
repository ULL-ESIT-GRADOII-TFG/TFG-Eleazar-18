{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Evaluate where

import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           Data.Text.Prettyprint.Doc.Render.Text
import           Lens.Micro.Platform
import           System.Console.Haskeline

import           Compiler.Ast
import           Compiler.Error
import           Compiler.Interpreter
import           Compiler.Interpreter.Command
import           Compiler.Parser.Methods
import           Compiler.Parser.Types
import           Compiler.Prettify
import           Compiler.Token.Lexer
import           Compiler.Types
import           Compiler.World


-- | Start an repl without prelude
repl :: Interpreter ()
repl = do
  prompt' <- getPrompt
  minput  <- lift . lift $ getInputLine prompt'
  case minput of
    Nothing    -> return ()
    Just input -> do
      mText <- use multilineA
      case mText of
        Just text
          | T.null . T.strip $ T.pack input -> do
            multilineA .= Nothing
            value <- compileSource text "**Interpreter**"
            docs <- liftWorld $ showObject value
            liftIO $ putDoc docs >> putStr "\n"

          | otherwise ->
            multilineA .= Just (text `mappend` "\n" `mappend` T.pack input)
        Nothing -> do
          tokenizer' <- tokenizer $ T.pack input
          case tokenizer' of
            Partial _ -> multilineA .= Just (T.pack input)
            Complete _ -> flip catchError handleREPLError $ do
              value <- compileSource (T.pack input) "**Interpreter**"
              docs <- liftWorld $ showObject value
              liftIO $ putDoc docs >> putStr "\n"
  repl

-- | Get prompt from configuration try execute prompt code else show a
-- default prompt with error flag
getPrompt :: Interpreter String
getPrompt = do
  isMultiline <- isJust <$> use multilineA
  flip catchError
    (\e -> do
      handleREPLError e
      return $
        if isMultiline then "[ERROR] ... " else "[ERROR] >>> ")
    $ do
      value <- liftWorld $ unwrap <$> getVarWithName "__prompt__"
      case value of
        OStr text ->
          if isMultiline then
            return $ replicate (T.length text - 4) ' ' ++ "... "
          else return $ T.unpack text
        OFunc{} -> do
          ostr <- liftWorld $ directCall value []
          case ostr of
            OStr text ->
              if isMultiline then
                return $ replicate (T.length text - 4) ' ' ++ "... "
              else return $ T.unpack text
            _ ->
              throwError $ Internal "Not a string value"
        obj ->
          throwError $ Internal $ "Not promptable value: " <> typeName obj

-- | Handle possible errors during execution of interpreter
-- TODO: Improve
handleREPLError :: InterpreterError -> Interpreter ()
handleREPLError err = case err of
  Tokenizer err'     -> liftIO $ T.putStrLn err'
  Parsing parseError -> liftIO . putStrLn $ show parseError
  Internal  err'     -> liftIO $ T.putStrLn err'
  WrapWorld err'     -> liftIO $ putDoc (renderError err') >> putStr "\n"


-- | Compile source code
compileSource :: T.Text -> String -> Interpreter Object
compileSource rawFile nameFile = do
  ast <- catchEither id . return $ generateAST rawFile nameFile
  verbosity <- use verboseLevelA
  when (verbosity > 2) $ do
    liftIO $ putStrLn "** AST **"
    liftIO $ putDoc $ prettify ast verbosity
  case ast of
    Command cmd args -> executeCommand cmd args >> return ONone
    Code statements  -> do
      astScoped <- liftWorld . liftScope $
        computeStatements statements
      when (verbosity >= 2) $ do
        liftIO $ putStrLn "** SCOPED AST **"
        mapM_ (liftIO . putDoc . flip prettify verbosity) astScoped
      foldM (\_ ast' -> evaluateScopedProgram ast') ONone astScoped

-- | Evaluate program with AST already scoped
evaluateScopedProgram :: Expression ScopeInfoAST -> Interpreter Object
evaluateScopedProgram astScoped = do
  verbosity <- use verboseLevelA
  instrs <- liftWorld . liftScope $ transform astScoped
  when (verbosity >= 2) $ do
    liftIO $ putStrLn "** Instructions **"
    liftIO $ putDoc $ prettify instrs verbosity
  liftWorld (runProgram instrs)

-- | First phase of interpreter
tokenizer :: T.Text -> Interpreter Tokenizer
tokenizer input = case scanner False $ T.unpack input of
  Left err -> do
    liftIO $ putStrLn err
    return $ Complete mempty
  Right toks -> return toks

generateAST :: T.Text -> String -> Either InterpreterError Repl
generateAST rawFile nameFile = do
  tokenizer' <- first (Tokenizer . T.pack) . scanner True $ T.unpack rawFile
  first Parsing . parserLexer nameFile $ getTokens tokenizer'

-- | Computar las class y los import, unir todos los Expression con seq
computeStatements :: [Statement TokenInfo] -> ScopeM [Expression ScopeInfoAST]
computeStatements stmts = reverse <$> foldM
  (\exprs st -> do
    expr <- transform st
    return $ expr:exprs) [] stmts