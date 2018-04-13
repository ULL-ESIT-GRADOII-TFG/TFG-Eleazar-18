{-# LANGUAGE OverloadedStrings #-}
module Compiler.Interpreter.Methods where

import           Data.Maybe
import qualified Data.Text as T
import           Data.Bifunctor
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Lens.Micro.Platform
import           System.Console.Haskeline

import Compiler.Ast
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
              showable <- showInterpreter value
              liftIO $ putStrLn showable
            Left err -> liftIO $ print err

-- | Prelude load action
loadPrelude :: Interpreter ()
loadPrelude = mapM_ (uncurry newVar) baseObjects

-- | Allow execute actions from ScopeM into Interpreter
liftScope :: ScopeM b -> Interpreter (Either ScopeError b)
liftScope scopeM = do
  lastScope <- use (memory.scope)
  let (value, newScope) = runState (runExceptT scopeM) lastScope
  memory.scope .= newScope
  return value

-- | Allow execute actions from StWorld into Interpreter
liftWorld :: StWorld a -> Interpreter a
liftWorld stWorld = do
  lastMemory         <- use memory
  (value, newMemory) <- liftIO $ runStateT stWorld lastMemory
  memory .= newMemory
  return value

-- | Internal use. To create native objects
newVar :: T.Text -> Object -> Interpreter ()
newVar idName obj = do
  eRef <- liftScope $ addNewIdentifier [idName]
  case eRef of
    Right ref -> liftWorld $ addObject ref obj
    Left  err -> liftIO $ print err

-- | Internal use to get specific interpreter variables
getVar :: T.Text -> Interpreter Object
getVar idName = do
  eRef <- liftScope $ getIdentifier [idName]
  case eRef of
    Right ref -> liftWorld $ findVar ref
    Left  err -> liftIO $ print err >> return ONone

tryExecuteICommand :: Repl -> Interpreter (Maybe [Statement TokenInfo])
tryExecuteICommand (Command cmd args) =
  executeCommand cmd args >> return Nothing
tryExecuteICommand (Code statements) = return $ Just statements

-- TODO:
executeCommand :: T.Text -> [T.Text] -> Interpreter ()
executeCommand _ _ = do
  mem <- use memory
  liftIO $ print mem

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

showInterpreter :: Object -> Interpreter String
showInterpreter obj = case obj of
  (OStr str) -> return $ "\"" ++ T.unpack str ++ "\""
  (ORegex str) -> return $ "/" ++ T.unpack str ++ "/"
  (OShellCommand str) -> return $ "$ " ++ T.unpack str
  (ODouble val) -> return $ show val
  (OBool val) -> return $ show val
  (ONum val) -> return $ show val
  (ORef rfs) -> liftWorld (follow rfs) >>= showInterpreter <&> ("*-> " ++)
  ONone -> return "None"
  object -> return $ show object
