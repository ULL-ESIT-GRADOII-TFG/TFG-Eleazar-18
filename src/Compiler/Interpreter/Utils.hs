module Compiler.Interpreter.Utils where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import           Compiler.Scope.Utils
import           Compiler.Types
import           Compiler.World.Methods


-- | Allow execute actions from StWorld into Interpreter
liftWorld :: StWorld a -> Interpreter a
liftWorld stWorld = do
  lastMemory <- use memoryA
  values <- liftIO $ runExceptT (runStateT stWorld (TkSt lastMemory def))
  case values of
    Right (value, newMemory) -> do
      memoryA .= newMemory^.innerStateA
      return value
    Left err -> throwError $ WrapWorld err

-- | Catch `Maybe` error into interpreter to handle apart
catchMaybe :: InterpreterError -> Interpreter (Maybe a) -> Interpreter a
catchMaybe errorMsg m = do
  option <- m
  case option of
    Just a  -> return a
    Nothing -> throwError errorMsg

-- | Catch `Either` error into interpreter to handle apart
catchEither :: (a -> InterpreterError) -> Interpreter (Either a b) -> Interpreter b
catchEither errorMsg m = do
  option <- m
  case option of
    Right a  -> return a
    Left err -> throwError $ errorMsg err

-- | Internal use. To create native objects
newVar :: T.Text -> Object -> Interpreter AddressRef
newVar idName obj = do
  addr <- liftWorld . liftScope . addNewIdentifier $ return idName
  _ <- liftWorld $ addObject addr obj
  return addr

-- | Internal use to get specific interpreter variables
getVar :: T.Text -> Interpreter Object
getVar idName = do
  addr <- liftWorld $ liftScope . getIdentifier $ return idName
  liftWorld $ findObject addr
