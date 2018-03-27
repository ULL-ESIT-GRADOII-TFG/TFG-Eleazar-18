{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Methods where

import           Data.Maybe
import qualified Data.Text as T
import qualified Text.Parsec as P
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Lens.Micro.Platform
import           System.Console.Haskeline

import Compiler.Interpreter.Types
import Compiler.Object.Types
import Compiler.World.Types
import Compiler.World.Methods
import Compiler.Instruction.Methods
import Compiler.Scope.Types
import Compiler.Scope.Methods
import Compiler.Parser.Types
import Compiler.Parser.Methods
import Compiler.Prelude.Methods
import Compiler.Token.Lexer (scanner, getTokens, Tokenizer(..))


-- | Initial State of interpreter
initialState :: IState
initialState = IState
  { _multiline    = Nothing
  , _memory = World
    { _table = mempty
    }
  , _scope = initialScope TokenInfo
  }

-- | Start an repl without prelude
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

-- | First phase of interpreter
tokenizer :: T.Text -> Interpreter Tokenizer
tokenizer input =
  case scanner False $ T.unpack input of
    Left err -> do
      liftIO $ putStrLn err
      return $ Complete mempty
    Right tokens -> return tokens

-- | Compile a file
compileFile :: T.Text -> P.SourceName -> Interpreter ()
compileFile rawFile name =
  case scanner True $ T.unpack rawFile of
    Left err -> liftIO $ putStrLn err
    Right tokenizer -> do
      liftIO $ print tokenizer
      case P.parse parseExp name (getTokens tokenizer) of
        Left err -> liftIO $ print err
        Right ast -> do
          liftIO $ print ast
          astScoped <- liftScope $ scopingThroughtAST ast
          case astScoped of
            Right scopeAst -> do
              value <- liftWorld (runProgram (astToInstructions scopeAst))
              liftIO $ print value
            Left err -> liftIO $ print err

-- | Prelude load action
loadPrelude :: Interpreter ()
loadPrelude = mapM_ (uncurry newVar) baseObjects

-- | Allow execute actions from ScopeM into Interpreter
liftScope :: ScopeM TokenInfo b -> Interpreter (Either ScopeError b)
liftScope scopeM = do
  lastScope <- use scope
  let (value, newScope) = runState (runExceptT scopeM) lastScope
  scope .= newScope
  return value

-- | Allow execute actions from StWorld into Interpreter
liftWorld :: StWorld a -> Interpreter a
liftWorld stWorld = do
  lastMemory <- use memory
  (value, newMemory) <- liftIO $ runStateT stWorld lastMemory
  memory .= newMemory
  return value

-- | Internal use. To create native objects
newVar :: T.Text -> Object -> Interpreter ()
newVar idName obj = do
  eRef <- liftScope $ addNewIdentifier idName
  case eRef of
    Right ref -> liftWorld $ addObject ref obj
    Left err -> liftIO $ print err

-- | Internal use to get specific interpreter variables
getVar :: T.Text -> Interpreter Object
getVar idName = do
  eRef <- liftScope $ getIdentifier idName
  case eRef of
    Right ref -> liftWorld $ findVar ref
    Left err -> liftIO $ print err >> return ONone
