{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ScriptLang.Interpreter
  ( repl
  , IState(..)
  , multiline
  , programState
  ) where

import           Control.Monad.State.Strict
import           Data.Monoid
import qualified Data.Text                  as T
import           Lens.Simple
import qualified Text.Megaparsec            as P

import           ScriptLang.Language.AST
import           ScriptLang.Language.Lexer
import           ScriptLang.Language.Parser
import           ScriptLang.Primitives
import           ScriptLang.Program

import           System.Console.Haskeline   (InputT, getInputLine, outputStrLn)

-- TODO: Parse error, al expected more items entrar en modo multilinea

--------------------------------------------------------------------------------
-- Interpreter main type
--------------------------------------------------------------------------------

-- | Used to control flow of all interpreter
type Interpreter = StateT IState (InputT IO)

-- | Represent internal state of compiler
data IState = IState
  { _multiline    :: !(Maybe T.Text)
   -- ^ When multiline mode is enable the interpreter storage all code
   --   until found a multiline close token. Then load text to compiler.
  , _programState :: !Env
  }

makeLenses ''IState

multiline :: Functor f => (Maybe T.Text -> f (Maybe T.Text)) -> IState -> f IState
programState :: Functor f => (Env -> f Env) -> IState -> f IState

repl :: Interpreter ()
repl = do
  prim <- use (programState.implicit)
  let prompt = if isPNone prim then " >>> " else "[" ++ T.unpack (infoPrim prim) ++ "] >>> "
  minput <- lift $ getInputLine prompt
  case minput of
    Nothing    -> return ()
    Just input ->
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

-- | Interpret specific commands of interpreter
astToIntepreter :: Show a => REPL a -> Interpreter ()
astToIntepreter Exit = return ()
astToIntepreter Help = lift (outputStrLn "Type :help to show this help") >> repl -- TODO:
astToIntepreter MultilineOpen = multilineMode
astToIntepreter MultilineClose = lift (outputStrLn "Maybe you wanna type :{ first") >> repl
astToIntepreter (Code prog) = execProgram prog >> repl

-- | When multiline mode is on
multilineMode :: Interpreter ()
multilineMode = do
  minput <- lift $ getInputLine "... "
  lift $ outputStrLn ""
  case minput of
    Nothing    -> return ()
    Just input ->
      case P.runParser endMultiline "Interpreter - Lexer" (T.pack input) of
        Left _ -> do
          let input' = T.pack input
          multiline %= (Just . maybe input' (input' <>))
          multilineMode
        Right _  -> do
          code <- use multiline
          case code of
            Just code' -> do
              prog <- generateProgram code' "Multiline Code"
              execProgram prog
              multiline .= Nothing
            Nothing -> return ()
          repl
  where
    endMultiline :: P.Parsec P.Dec T.Text String
    endMultiline = P.skipMany P.spaceChar >> P.string ":}"

generateProgram :: T.Text -> String -> Interpreter (Program ())
generateProgram text filename =
  case P.runParser parseTokens (filename <> " - Lexer") text of
    Left err -> do
      lift . outputStrLn $ P.parseErrorPretty err
      return []
    Right tokens' ->
      case P.runParser parseLanguage (filename <> " - AST") tokens' of
        Left err -> do
          --lift $ outputStrLn (P.parseErrorPretty err)
          return []
        Right prog -> return prog

execProgram :: Program a -> Interpreter ()
execProgram prog = do
  st <- use programState
  newState <- liftIO $ execStateT (interpretProgram prog) st
  programState .= newState
