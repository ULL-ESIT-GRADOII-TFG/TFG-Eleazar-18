module Compiler.Core where

import           Control.Monad.State.Strict
import           System.Console.Haskeline
import qualified Data.Text as T

import           Compiler.Interpreter.Types
import           Compiler.Interpreter.Methods
import           Compiler.World.Types


initialState :: IState
initialState = IState
  { _multiline    = Nothing
  , _programState = World
    { _table = mempty
    }
  }


-- | Start the program searching a config with authentication tokens,
--   or request to user to log in
start :: IO ()
start = runInputT defaultSettings . flip evalStateT initialState $ repl

repl :: Interpreter ()
repl = do
  {-
  prim <- use (programState . env . implicit)
  let prompt = if isNone prim
        then " >>> "
        else "[" ++ T.unpack (infoPrim prim) ++ "] >>> "
  -}
  let prompt = ">>> "
  minput <- lift $ getInputLine prompt
  case minput of
    Nothing    -> return ()
    Just input -> do
      lift . lift $ compileFile (T.pack input) "**Interpreter**"
      repl
      {-
      case P.runParser parseInterpreterTokens "Interpreter - Lexer" (T.pack input) of
        Left err -> lift . outputStrLn $ P.parseErrorPretty err
        Right lexer -> do
          lift $ outputStrLn ("\nLexer " ++ show lexer)
          case P.runParser parseInterpreter "Interpreter - AST" lexer of
            Left err ->
              lift $ outputStrLn "Error parsing AST"
              --lift . outputStrLn $ P.parseErrorPretty err
            Right astTree -> do
              lift $ outputStrLn ("AST" ++ show astTree)
              astToIntepreter astTree
      -}
