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
  lastMemory <- use memory
  values <- liftIO $ runExceptT (runStateT stWorld lastMemory)
  case values of
    Right (value, newMemory) -> do
      memory .= newMemory
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

-- | String representation of objects in REPL
showInterpreter :: Object -> Interpreter String
showInterpreter obj = case obj of
  OStr str -> return $ "\"" ++ T.unpack str ++ "\""
  ORegex str -> return $ "/" ++ T.unpack str ++ "/"
  OShellCommand str -> return $ "$ " ++ T.unpack str
  ODouble val -> return $ show val
  OBool val -> return $ show val
  ONum val -> return $ show val
  OVector vec -> return $ show vec -- TODO: Show Innervalue
  ORef rfs -> do
    obj <- liftWorld (follow rfs)
    showInterpreter obj <&> ("*-> " ++)
  ONone -> return "None"
  object -> return $ show object -- TODO: __print__
