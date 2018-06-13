{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
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
import           Control.Monad.Free.TH
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Free
import           Data.Default
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Vector                as V
import           Lens.Micro.Platform        hiding (assign)
import           System.Console.Haskeline   (InputT)
import           Text.PrettyPrint
import           Text.Regex.PCRE.Light

import           Compiler.Instruction.Types
import           Compiler.Interpreter.Types
import           Compiler.Parser.Types
import           Compiler.Prettify
import           Compiler.Scope.Types
import           Compiler.Utils
import           Compiler.World.Types


-------------------------------------------------------------------------------
-- * Config's Types

data Config = Config
  { _commandShell :: Maybe FilePath
  , _defaultPath  :: Maybe FilePath
  , _prompt       :: Prompt
  , _modules      :: [T.Text]
  }

newtype Prompt = Prompt { _unPrompt :: ScopeM Object }

instance Default Config where
  def = Config
    { _commandShell = Nothing
    , _defaultPath = Nothing
    , _prompt = Prompt . return $ OStr ">>> "
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

instance Prettify Var where
  prettify (Var rc raw) _verbose =
    text "VarRC " <> text (show rc) <> text " " <> text (show raw)

instance Default Var where
  def = Var 1 ONone

data World = World
  { _table        :: IM.IntMap Var
  , _scope        :: ScopeInfo
  -- ^ Root Scope.
  , _debugProgram :: (LT.Text, Int)
  -- TODO: This isn't the best way to solve this problem
  }
  deriving Show

instance Prettify World where
  prettify (World tb scope _) verbose =
    text "World {" $$
    nest 2 (vcat (map (\(k, v) ->
      text "#" <> text (show k) <> text " " <> prettify v verbose) $ IM.toList tb)) $$
    -- if verbose >= 3 then undefined else (empty <>)
    text "}"


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

-- | Defines stack scope for program
data Scope = Scope
  { _currentScope :: ScopeInfo
  -- ^ New variables declares in the current scope to be later added to
  -- stackScope
  , _stackScope   :: [ScopeInfo]
  -- ^ All above scopes
  } deriving Show

instance Default Scope where
  def = Scope
    { _currentScope = ScopeInfo mempty
    -- $ M.fromList [("__new__", AddressRef 0 [])]  -- MOVE: To Prelude
    , _stackScope   = []
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
  | ORegex Regex
  -- ^ Regex expression following pcre syntax
  | OShellCommand T.Text
  -- ^ Shell command
  | OVector (V.Vector Object)
  -- ^ Sequence of objects
  | OFunc (M.Map T.Text Object) [Word] Prog
  -- ^ Lambda with possible scope/vars attached
  | OObject (Maybe Word) (M.Map T.Text Word)
  -- ^ Object instance from class Word
  | ONative ([Object] -> Prog)
  -- ^ Native object
  | ORef Word
  -- ^ Pointer reference
  | OClassDef
    { nameClass       :: T.Text
    , attributesClass :: M.Map T.Text Object
    }
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
    OClassDef{}     -> "[ClassDef]"
    ONone           -> "[None]"


-------------------------------------------------------------------------------
-- * Instruction's Types
data Info = Info
  { _retrieveName :: IM.IntMap T.Text
  , _srcInfo      :: TokenInfo
  } deriving Show

type Prog = FreeT Instruction StWorld Object

-- | Intermediate set of instructions.
data Instruction next
  = CallCommand !Info !AddressRef ![Object] (Object -> next)
  -- ^ Make a call to and defined function
  | Assign !Info !AddressRef !Object (Object -> next)
  -- ^ Assign an object to local variable
  | DropVar !Info !AddressRef next
  -- ^ Remove a var from memory
  | GetVal !Info !AddressRef (Object -> next)
  -- ^ Retrieve a object from a memory reference
  | Loop !Info !Object (Object -> Prog) next
  -- ^ Loop over a object
  | Cond !Info !Object
      Prog
      Prog
      (Object -> next)
  -- ^ If sentence given a object
  deriving Functor


makeSuffixLenses ''IState
makeSuffixLenses ''Var
makeSuffixLenses ''World
makeSuffixLenses ''Config
makeSuffixLenses ''Prompt
makeSuffixLenses ''Scope
makeSuffixLenses ''ScopeInfo
makeSuffixLenses ''ScopeInfoAST
makeFree ''Instruction
