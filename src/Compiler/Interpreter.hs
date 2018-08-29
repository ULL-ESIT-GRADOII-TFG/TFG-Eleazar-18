{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Interpreter where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.Text                  as T
import           Lens.Micro.Platform
import           System.Console.Haskeline
import           Text.Parsec

import           Compiler.Config
import           Compiler.Error
import           Compiler.Types
import           Compiler.Utils
import           Compiler.World             ()


data InterpreterError
  = Tokenizer T.Text
  | Parsing ParseError
  | Internal T.Text
  | WrapWorld (ErrorInfo WorldError)
  deriving Show

-- | Used to control flow of all interpreter
type Interpreter = ExceptT InterpreterError (StateT IState (InputT IO))


-- | Represent internal state of compiler
data IState = IState
  { _multiline    :: !(Maybe T.Text)
   -- ^ When multiline mode is enable the interpreter storage all code
   --   until found a multiline close token. Then load text to compiler.
  , _memory       :: !(World Object)
  -- , _docs :: Docs -- Map Text DocFormatted
  , _config       :: !Config
  , _verboseLevel :: !Int
  }

makeSuffixLenses ''IState

instance Default IState where
  def = IState
    { _multiline = Nothing
    , _memory    = def
    , _config    = def
    , _verboseLevel = 0
    }

-- | Allow execute actions from StWorld into Interpreter
liftWorld :: StWorld a -> Interpreter a
liftWorld stWorld = do
  lastMemory <- use memoryA
  values <- liftIO $ runExceptT (runStateT stWorld lastMemory)
  case values of
    Right (value, newMemory) -> do
      memoryA .= newMemory
      return value
    Left err -> throwError $ WrapWorld err

-- | Catch `Maybe` error into interpreter to handle apart
catchMaybe :: InterpreterError -> Interpreter (Maybe a) -> Interpreter a
catchMaybe errorMsg m = do
  option' <- m
  case option' of
    Just a  -> return a
    Nothing -> throwError errorMsg

-- | Catch `Either` error into interpreter to handle apart
catchEither :: (a -> InterpreterError) -> Interpreter (Either a b) -> Interpreter b
catchEither errorMsg m = do
  option' <- m
  case option' of
    Right a  -> return a
    Left err -> throwError $ errorMsg err