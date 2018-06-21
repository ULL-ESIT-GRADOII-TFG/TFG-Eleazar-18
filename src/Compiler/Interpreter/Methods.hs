{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Methods where

import           Control.Monad.Except
import           Data.Maybe
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Text.Lazy.IO                    as LT
import           Lens.Micro.Platform
import           System.Console.Haskeline
import           Text.PrettyPrint                     (renderStyle, style)

import           Compiler.Ast
import           Compiler.Desugar.Types
import           Compiler.Error
import           Compiler.Instruction.Methods
import           Compiler.Interpreter.Command.Methods
import           Compiler.Interpreter.Utils
import           Compiler.Object.Methods
import           Compiler.Parser.Methods
import           Compiler.Prettify
import           Compiler.Scope.Methods               ()
import           Compiler.Token.Lexer                 (Tokenizer (..), scanner)
import           Compiler.Types
import           Compiler.World.Methods


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
            compileSource text "**Interpreter**"
          | otherwise ->
            multilineA .= Just (text `mappend` "\n" `mappend` T.pack input)
        Nothing -> do
          tokenizer' <- tokenizer $ T.pack input
          case tokenizer' of
            Partial _ -> multilineA .= Just (T.pack input)
            Complete _ ->
              compileSource (T.pack input) "**Interpreter**"
                `catchError` handleREPLError
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
      pt <- use (configA.promptA.unPromptA)
      value <- liftWorld $ liftScope pt
      case value of
        OStr text ->
          if isMultiline then
            return $ replicate (T.length text - 4) ' ' ++ "... "
          else return $ T.unpack text
        OFunc{} -> do
          ostr <- liftWorld $ callObjectDirect value []
          case ostr of
            OStr text ->
              if isMultiline then
                return $ replicate (T.length text - 4) ' ' ++ "... "
              else return $ T.unpack text
            _ ->
              throwError $ Internal "Not a string value"
        _ ->
          -- TODO: Maybe cast object to string object
          throwError $ Internal "Not a string value"

-- | Handle possible errors during execution of interpreter
-- TODO: Improve
handleREPLError :: InterpreterError -> Interpreter ()
handleREPLError err = case err of
  Compiling err' -> liftIO $ T.putStrLn err'
  Internal  err' -> liftIO $ T.putStrLn err'
  WrapWorld err' -> liftIO $ putStrLn $ renderStyle style $ renderError err'


-- | First phase of interpreter
tokenizer :: T.Text -> Interpreter Tokenizer
tokenizer input = case scanner False $ T.unpack input of
  Left err -> do
    liftIO $ putStrLn err
    return $ Complete mempty
  Right tokens -> return tokens

-- | Compile source code
compileSource :: T.Text -> String -> Interpreter ()
compileSource rawFile nameFile = do
  ast <- catchEither id . return $ generateAST rawFile nameFile
  verbosity <- use verboseLevelA
  when (verbosity > 2) $ do
    liftIO $ putStrLn "** AST **"
    liftIO $ putStrLn $ renderStyle style $ prettify ast verbosity
  case ast of
    Command cmd args -> executeCommand cmd args
    Code statements  -> do
      astScoped <- liftWorld . liftScope $
        computeStatements statements
      when (verbosity >= 2) $ do
        liftIO $ putStrLn "** SCOPED AST **"
        mapM_ (liftIO . putStrLn . renderStyle style . flip prettify verbosity) astScoped
      mapM_ evaluateScopedProgram astScoped

compileSourcePure :: T.Text -> String -> Either InterpreterError (ScopeM Prog)
compileSourcePure rawFile nameFile = do
  ast <- generateAST rawFile nameFile
  case ast of
    Command _cmd _args -> Left $ Compiling "You can't use command"
    Code statements    -> Right $ computeStatements statements >>= transform . ExprSeq

-- | Evaluate program with AST already scoped
evaluateScopedProgram :: Expression ScopeInfoAST -> Interpreter ()
evaluateScopedProgram astScoped = do
  verbosity <- use verboseLevelA
  instrs <- liftWorld . liftScope $ transform astScoped
  when (verbosity >= 2) $ do
    liftIO $ putStrLn "** Instructions **"
    instrsPP <- liftWorld $ do
      _ <- pprint instrs
      instrsPP <- use $ innerStateA.debugProgramA._1
      innerStateA.debugProgramA .= ("", 0)
      return instrsPP
    liftIO $ LT.putStrLn instrsPP
  value <- liftWorld (runProgram instrs)
  showable <- showInterpreter value
  liftIO $ putStrLn showable

-- | Computar las class y los import, unir todos los Expression con seq
computeStatements :: [Statement TokenInfo] -> ScopeM [Expression ScopeInfoAST]
computeStatements stmts = reverse <$> foldM
  (\exprs st -> do
    expr <- transform st
    return $ expr:exprs) [] stmts
