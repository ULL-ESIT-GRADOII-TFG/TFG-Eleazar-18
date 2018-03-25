{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Methods where

import           Data.Maybe
import qualified Data.Text as T
import qualified Text.Parsec as P
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Lens.Micro.Platform
import           System.Console.Haskeline

import Compiler.Interpreter.Types
import Compiler.World.Types
import Compiler.Parser.Types
import Compiler.Instruction.Methods
import Compiler.Scope.Methods
import Compiler.Parser.Methods
import Compiler.Token.Lexer (scanner, getTokens, Tokenizer(..))


initialState :: IState
initialState = IState
  { _multiline    = Nothing
  , _memory = World
    { _table = mempty
    }
  , _scope = initialScope TokenInfo
  }

repl :: Interpreter ()
repl = do
  {-
  prim <- use (programState . env . implicit)
  let prompt = if isNone prim
        then " >>> "
        else "[" ++ T.unpack (infoPrim prim) ++ "] >>> "
  -}
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
          | otherwise             -> do
            multiline .= Just (text `mappend` "\n" `mappend` (T.pack input))
        Nothing -> do
          tokenizer' <- tokenizer $ T.pack input
          case tokenizer' of
            Partial _ -> multiline .= Just (T.pack input)
            Complete _ -> compileFile (T.pack input) "**Interpreter**"
  repl

tokenizer :: T.Text -> Interpreter Tokenizer
tokenizer input =
  case scanner False $ T.unpack input of
    Left err -> do
      lift . lift $ putStrLn err
      return $ Complete mempty
    Right tokens -> return tokens

compileFile :: T.Text -> P.SourceName -> Interpreter ()
compileFile rawFile name =
  case scanner True $ T.unpack rawFile of
    Left err -> lift . lift $ putStrLn err
    Right tokenizer -> do
      lift . lift $ print tokenizer
      case P.parse parseExp name (getTokens tokenizer) of
        Left _ -> return ()
        Right ast -> do
          lift . lift $ print ast
          lastScope <- use scope
          let (astScoped, newScope) = runState (runExceptT (scopingThroughtAST ast)) lastScope
          scope .= newScope
          case astScoped of
            Right scopeAst -> do
              lastMemory <- use memory
              (value, newMemory) <- lift.lift $ runStateT (runProgram (astToInstructions scopeAst)) lastMemory
              lift . lift $ print value
              memory .= newMemory
              return ()
            Left err -> lift . lift $ print err
