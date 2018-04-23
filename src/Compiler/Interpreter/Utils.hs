module Compiler.Interpreter.Utils where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import           Compiler.Instruction.Types
import           Compiler.Interpreter.Types
import           Compiler.Object.Types
import           Compiler.Scope.Methods
import           Compiler.Scope.Types
import           Compiler.World.Methods
import           Compiler.World.Types


-- | Allow execute actions from ScopeM into Interpreter
liftScope :: ScopeM b -> Interpreter (Either ScopeError b)
liftScope scopeM = do
  lastScope <- use (memory.scope)
  let (value, newScope) = runState (runExceptT scopeM) lastScope
  memory.scope .= newScope
  return value

-- | Allow execute actions from StWorld into Interpreter
liftWorld :: StWorld a -> Interpreter (Maybe a)
liftWorld stWorld = do
  lastMemory <- use memory
  values <- liftIO $ runExceptT (runStateT stWorld lastMemory)
  case values of
    Right (value, newMemory) -> do
      memory .= newMemory
      return $ Just value
    Left err -> liftIO $ print err >> return Nothing

-- | Catch `Maybe` error into interpreter to handle apart
catchMaybe :: InterpreterError -> Interpreter (Maybe a) -> Interpreter a
catchMaybe errorMsg m = do
  option <- m
  case option of
    Just a -> return a
    Nothing -> throwError errorMsg

-- | Catch `Either` error into interpreter to handle apart
catchEither :: (a -> InterpreterError) -> Interpreter (Either a b) -> Interpreter b
catchEither errorMsg m = do
  option <- m
  case option of
    Right a -> return a
    Left err -> throwError $ errorMsg err

-- | Internal use. To create native objects
newVar :: T.Text -> Object -> Interpreter (Maybe AddressRef)
newVar idName obj = do
  eRef <- liftScope $ addNewIdentifier [idName]
  case eRef of
    Right ref' -> do
      _ <- liftWorld $ addObject ref' obj
      return $ Just ref'
    Left  err -> do
      liftIO $ print err
      return Nothing

-- | Internal use to get specific interpreter variables
getVar :: T.Text -> Interpreter Object
getVar idName = do
  eRef <- liftScope $ getIdentifier [idName]
  case eRef of
    Right ref' -> do
      mObj <- liftWorld $ findObject ref'
      case mObj of
        Just obj -> return obj
        Nothing  -> return ONone
    Left  err -> liftIO $ print err >> return ONone

-- | String representation of objects in REPL
showInterpreter :: Object -> Interpreter String
showInterpreter obj = case obj of
  OStr str -> return $ "\"" ++ T.unpack str ++ "\""
  ORegex str -> return $ "/" ++ T.unpack str ++ "/"
  OShellCommand str -> return $ "$ " ++ T.unpack str
  ODouble val -> return $ show val
  OBool val -> return $ show val
  ONum val -> return $ show val
  ORef rfs -> do
    mObj <- liftWorld (follow rfs)
    case mObj of
      Just obj' -> showInterpreter obj' <&> ("*-> " ++)
      Nothing  -> return ""
  ONone -> return "None"
  object -> return $ show object -- TODO: __print__
