{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.Vector                as V
import           Lens.Micro.Platform        hiding (assign)
import           System.Console.Haskeline   (InputT)
import           Text.PrettyPrint
import           Text.Regex.PCRE.Light

import           Compiler.Error
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

-- | Note: `ExceptT` wraps monad state, in case of fail discard memory. That it
-- is the predicate to follow.
type StWorld = StateT (TkSt World) (ExceptT (ErrorInfo WorldError) IO)

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

-- | Keeps all information of running program (memory, debugging info ...)
data World = World
  { _table :: IM.IntMap Var
  -- ^ Generic table to storage all vars/objects
  , _scope :: Scope
  -- ^ Root Scope.
  , _co    :: [Word]
  -- ^ Object Scope
  }
  deriving Show

instance Prettify World where
  prettify (World tb scope _) verbose =
    let aux = leftInnerJoin ((M.toList (_renameInfo $ _currentScope scope)) & each._2 %~ (fromIntegral . _ref)) (IM.toList tb)
    in
      text "World {" $$
      -- Do a cross joint
      nest 2 (vcat (map (\(name, address, value) ->
        text (T.unpack name)
        <> text " -> "
        <> text "#"
        <> text (show address)
        <> text " "
        <> prettify value verbose) aux)) $$
      text "}"

instance Default World where
  def = World
    { _table = mempty
    , _scope = def
    , _co = []
    }


-------------------------------------------------------------------------------
-- * Scope

type ScopeM = ExceptT (ErrorInfo ScopeError) (TkState Scope IO)

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
  | OFunc (M.Map T.Text Object) [Word] ([Object] -> Prog)
  -- ^ Lambda with possible scope/vars attached
  | OObject (Maybe Word) (M.Map T.Text Word)
  -- ^ Object instance from class Word
  | ONative ([Object] -> Prog)
  -- ^ Native object
  | ORef Word
  -- ^ Pointer reference
  | OClassDef
    { nameClass       :: T.Text
    , refClass        :: Word
    , attributesClass :: M.Map T.Text Word
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


-------------------------------------------------------------------------------
-- * Custom State type to kkep track of TokenInfo
type TkState st m = StateT (TkSt st) m

-- | Intermediate data type to keeps tracks of current TokenInfo
data TkSt a = TkSt
  { _innerState       :: a
  , _currentTokenInfo :: TokenInfo
  -- ^ Used to generate precise errors locations
  } deriving Show

instance Default a => Default (TkSt a) where
  def = TkSt def def

instance Monad m => GetInfo (StateT (TkSt st) m) where
  getInfo = _currentTokenInfo <$> get

makeSuffixLenses ''IState
makeSuffixLenses ''Var
makeSuffixLenses ''World
makeSuffixLenses ''Config
makeSuffixLenses ''Prompt
makeSuffixLenses ''Scope
makeSuffixLenses ''ScopeInfo
makeSuffixLenses ''ScopeInfoAST
makeSuffixLenses ''TkSt
makeFree ''Instruction
