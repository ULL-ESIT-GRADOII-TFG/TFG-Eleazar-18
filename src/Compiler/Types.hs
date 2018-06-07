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
import           Text.PrettyPrint

import           Compiler.Instruction.Types
import           Compiler.Interpreter.Types
import           Compiler.Parser.Types
import           Compiler.Prettify
import           Compiler.Scope.Types
import           Compiler.World.Types


-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
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


-------------------------------------------------------------------------------
-- * World's Types

-- | Used to storage vars into memory, atleast its reference structure
data Var = Var
  { _refCounter :: !Word
  , _rawObj     :: !Object
  }
  deriving Show

instance Default Var where
  def = Var 1 ONone

data World = World
  { _table        :: IM.IntMap Var
  , _scope        :: Scope
  , _debugProgram :: (LT.Text, Int)
  }
  deriving Show


instance Default World where
  def = World
    { _table = mempty
    , _scope = def
    , _debugProgram = ("", 0)
    }

-- | Note: `ExceptT` wraps monad state, in case of fail discard memory. That it
-- is the predicate to follow.
type StWorld = StateT World (ExceptT WorldError IO)


-------------------------------------------------------------------------------
-- * Scope

type ScopeM = ExceptT ScopeError (StateT Scope IO)

data Scope = Scope
  { _nextId          :: Word
  -- ^ Unique identifier. This is temporary. Needs to be change to something better
  , _currentScope    :: ScopeInfo
  -- ^ New variables declares in the current scope to be later added to stackScope
  , _stackScope      :: [ScopeInfo]
  -- ^ All above scopes
  , _scopeAst        :: ScopeInfo
  -- ^ Used to generate ScopeInfoAST
  , _typeDefinitions :: IM.IntMap ClassDefinition
  -- ^ Scheme for object class
  } deriving Show

instance Default Scope where
  def = Scope
    { _nextId       = 0
    , _currentScope = ScopeInfo $ M.fromList [("__new__", AddressRef 0 [])]
    , _stackScope   = []
    , _scopeAst     = def
    , _typeDefinitions = mempty
    }

newtype ScopeInfo = ScopeInfo
  { _renameInfo :: M.Map T.Text AddressRef
  } deriving Show

instance Default ScopeInfo where
  def = ScopeInfo mempty

instance Prettify ScopeInfo where
  prettify (ScopeInfo hash) verbose =
    text "ScopeInfo { " $$
    nest 2 (vcat (map (\(k,v) ->
      text (T.unpack k) <> text " -> " <> prettify v verbose) $ M.toList hash)) $$
    text "}"

data ClassDefinition = ClassDefinition
  { _nameClass       :: T.Text
  , _attributesClass :: M.Map T.Text Object
  -- ^ Methods of class to be used with objects instanced
  }
  deriving Show

data ScopeInfoAST = ScopeInfoAST
  { _tokenInfo :: TokenInfo
  , _scopeInfo :: ScopeInfo
  } deriving Show

instance Default ScopeInfoAST where
  def = ScopeInfoAST def def

instance Prettify ScopeInfoAST where
  prettify scopeInfoAST verbose =
    text "ScopeInfoAST { " $$
    nest 2 (prettify (_tokenInfo scopeInfoAST) verbose $$
            prettify (_scopeInfo scopeInfoAST) verbose) $$
    text "}"

-------------------------------------------------------------------------------
-- * Object's Types

data Object
  = OStr T.Text
  | OBool Bool
  | ODouble Double
  | ONum Int
  | ORegex T.Text -- TODO: search precompiled type
  | OShellCommand T.Text
  | OVector (V.Vector Object)
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
    ONone           -> "[None]"


-------------------------------------------------------------------------------
-- * Instruction's Types

type Prog = FreeT Instruction StWorld Object

-- | Intermediate set of instructions.
data Instruction next
  = CallCommand !AddressRef ![Object] (Object -> next)
  -- ^ Make a call to and defined function
  | Assign !AddressRef !Object (Object -> next)
  -- ^ Assign an object to local variable
  | DropVar !AddressRef next
  -- ^ Remove a var from memory
  | GetVal !AddressRef (Object -> next)
  -- ^ Retrieve a object from a memory reference
  | Loop !Object (Object -> Prog) next
  -- ^ Loop over a object
  | Cond !Object
      Prog
      Prog
      (Object -> next)
  -- ^ If sentence given a object
  deriving Functor

makeLenses ''IState
makeLenses ''Var
makeLenses ''World
makeLenses ''Config
makeLenses ''Prompt
makeLenses ''Scope
makeLenses ''ScopeInfo
makeLenses ''ScopeInfoAST
makeLenses ''ClassDefinition
