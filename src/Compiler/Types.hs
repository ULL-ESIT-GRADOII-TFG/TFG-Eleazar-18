{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Types
  ( module Compiler.Types
  , module Compiler.Instruction.Types
  , module Compiler.Interpreter.Types
  , module Compiler.Scope.Types
  , module Compiler.World.Types
  , module Compiler.Parser.Types
  , module Data.Default
  , module Lens.Micro.Platform
  ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Free
import           Data.Default
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Vector                as V
import           Lens.Micro.Platform
import           System.Console.Haskeline   (InputT)

import           Compiler.Instruction.Types
import           Compiler.Interpreter.Types
import           Compiler.Parser.Types
import           Compiler.Scope.Types
import           Compiler.World.Types


-- * Config's Types

data Config = Config
  { _commandShell :: Maybe FilePath
  , _defaultPath  :: Maybe FilePath
  , _prompt       :: Prompt
  , _modules      :: [T.Text]
  }

newtype Prompt = Prompt { _unPrompt :: ScopeM Prog }

instance Default Config where
  def = Config
    { _commandShell = Nothing
    , _defaultPath = Nothing
    , _prompt = Prompt . return . return $ OStr ">>> "
    , _modules =  []
    }

-- * Interpreter's Types

-- | Used to control flow of all interpreter
type Interpreter = ExceptT InterpreterError (StateT IState (InputT IO))

-- | Represent internal state of compiler
data IState = IState
  { _multiline    :: !(Maybe T.Text)
   -- ^ When multiline mode is enable the interpreter storage all code
   --   until found a multiline close token. Then load text to compiler.
  , _memory       :: !World
  -- , _docs :: Docs -- Map Text DocFormatted
  , _config       :: !Config
  , _verboseLevel :: !Int
  }

instance Default IState where
  def = IState
    { _multiline = Nothing
    , _memory    = def
    , _config    = def
    , _verboseLevel = 0
    }


-- * World's Types

-- | Used to storage vars into memory, atleast its reference structure
data Var = Var
  { _refCounter :: !Word
  , _rawObj     :: !Object
  }
  deriving Show

data World = World
  { _table        :: IM.IntMap Var
  , _scope        :: Scope
  , _debugProgram :: (LT.Text, Int)
  }
  deriving Show

instance Default World where
  def = World {_table = mempty, _scope = def, _debugProgram = ("", 0)}

-- | Note: `ExceptT` wraps monad state, in case of fail discard memory. That it
-- is the predicate to follow.
type StWorld = StateT World (ExceptT WorldError IO)


-- * Object's Types

data Object
  = OStr T.Text
  | OBool Bool
  | ODouble Double
  | ONum Int
  | ORegex T.Text -- TODO: search precompiled type
  | OShellCommand T.Text
  | OVector (V.Vector Object)
  | ODic (M.Map T.Text Object)
  | OFunc (M.Map T.Text Object) [Word] Prog
  | OObject (Maybe Word) (M.Map T.Text Word)
  -- ^ Object instance from class Word
  | ONative ([Object] -> Prog)
  -- ^ Native object
  | ORef Word
  -- ^ Pointer reference
  | ONone

instance Show Object where
  show obj = case obj of
    OStr{}          -> "[String]"
    OBool{}         -> "[Bool]"
    ODouble{}       -> "[Double]"
    ORegex{}        -> "[Regex]"
    OShellCommand{} -> "[ShellCmd]"
    ONum{}          -> "[Int]"
    OFunc{}         -> "[Function]"
    ONative{}       -> "[Native Function]"
    ORef{}          -> "[Reference]"
    OObject{}       -> "[Object]"
    OVector{}       -> "[Vector]"
    ODic{}          -> "[Dictionary]"
    ONone           -> "[None]"


-- * Instruction's Types

type Prog = FreeT Instruction StWorld Object

type Instruction = InstructionG StWorld

-- | Intermediate set of instructions.
data InstructionG (st :: * -> *) next
  = CallCommand !AddressRef ![Object] (Object -> next)
  -- ^ Make a call to and defined function
  | Assign !AddressRef !Object next
  -- ^ Assign an object to local variable
  | DropVar !AddressRef next
  -- ^ Remove a var from memory
  | GetVal !AddressRef (Object -> next)
  -- ^ Retrieve a object from a memory reference
  | Loop !Object (Object -> FreeT (InstructionG st) st Object) next
  -- ^ Loop over a object
  | Cond !Object
      (FreeT (InstructionG st) st Object)
      (FreeT (InstructionG st) st Object)
      (Object -> next)
  -- ^ If sentence given a object
  | End
  -- ^ End program, ignore all after that
  deriving Functor

makeLenses ''IState
makeLenses ''Var
makeLenses ''World
makeLenses ''Config
makeLenses ''Prompt
