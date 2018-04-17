module Compiler.Interpreter.Utils where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.Text                  as T
import           Lens.Micro.Platform

import           Compiler.Interpreter.Types
import Compiler.Instruction.Types
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
      Just obj -> showInterpreter obj <&> ("*-> " ++)
      Nothing  -> return ""
  ONone -> return "None"
  object -> return $ show object -- TODO: __print__
