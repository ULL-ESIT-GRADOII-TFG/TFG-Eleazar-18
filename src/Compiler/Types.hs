{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
module Compiler.Types where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import qualified Data.List.NonEmpty         as NL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector                as V
import           GHC.Exts
import           Text.Regex.PCRE.Light

import           Compiler.Error
import           Compiler.Parser.Types
import           Compiler.Prettify
import           Compiler.Utils


newtype ShellType = ShellType { unShell :: T.Text }

-- | Allows generation of new names, with its ids associated
class Monad sc => Naming sc where
  newId :: T.Text -> sc PathVar
  getNewId :: sc Address
  findAddress :: T.Text -> sc (Maybe PathVar)
  findAddress = findAddress' . return
  findAddress' :: NL.NonEmpty T.Text -> sc (Maybe PathVar)
  {-# MINIMAL newId, getNewId, findAddress' #-}

-- | Mainly used for debug propurses or type info display
class TypeName o where
  typeName :: o -> Text

newtype ScopeInfo = ScopeInfo
  { _renameInfo :: HM.HashMap T.Text PathVar
  } deriving Show

type ScopeM = ExceptT (ErrorInfo ScopeError) (StateT Scope IO)

-- | Defines stack scope for program
data Scope = Scope
  { _currentScope :: ScopeInfo
  -- ^ New variables declares in the current scope to be later added to
  -- stackScope
  , _stackScope   :: [ScopeInfo]
  -- ^ All above scopes
  } deriving Show


data ScopeInfoAST = ScopeInfoAST
  { _tokenInfo :: TokenInfo
  , _scopeInfo :: ScopeInfo
  } deriving Show

-------------------------------------------------------------------------------
-- * Memory relate types

type Address = Int

data PathVar = PathVar
  { _ref     :: Address
  , _dynPath :: [Text]
  }
  deriving Show

simple :: Address -> PathVar
simple addr = PathVar addr []

instance Prettify PathVar where
  prettify (PathVar r p) _verbose =
    "ADDR#" <> pretty r <> "." <> pretty (T.intercalate "." p)

class Applicative r => Wrapper r where
  wrap :: a -> r a
  wrap = pure
  unwrap :: r a -> a

class (Naming mm, Wrapper (Store mm))
    => MemoryAccessor (mm :: * -> *) o | mm -> o where
  type Store mm :: * -> *
  getVar :: Address -> mm (Store mm o)
  setVar :: Address -> Store mm o -> mm ()
  -- | Look into memory to find the final object pointed, and returns also its address
  -- Fails in case of not found the object `NotFoundObject`
  findPathVar :: PathVar -> mm (Store mm o, Address)
  setPathVar :: PathVar -> Store mm o -> mm ()


class Deallocate mm where
  deleteVar :: Address -> mm Bool
  deleteUnsafe :: Address -> mm ()

newVar :: MemoryAccessor mm Object => Store mm Object -> mm Address
newVar var = do
  addr <- getNewId
  setVar addr var
  return addr

newVarWithName :: MemoryAccessor mm Object => T.Text -> Object -> mm Address
newVarWithName nameId obj = do
  ref <- _ref <$> newId nameId
  setVar ref (pure obj)
  return ref

getVarWithName :: (MonadError (ErrorInfo WorldError) mm, GetInfo mm, MemoryAccessor mm Object) => T.Text -> mm (Store mm Object)
getVarWithName nameId = do
  mPathVar <- findAddress nameId
  case mPathVar of
    Just pathVar -> fst <$> findPathVar pathVar
    Nothing      -> throw (ScopeError (NotDefinedObject nameId))

type StWorld = StateT (World Object) (ExceptT (ErrorInfo WorldError) IO)

data Rc o = Rc
  { _refCounter :: !Address
  , _rawObj     :: !o
  }
  deriving Show


-- | Keeps all information of running program (memory, debugging info ...)
data World o = World
  { _table         :: IM.IntMap (Rc o)
  -- ^ Generic table to storage all vars/objects
  , _scope         :: Scope
  -- ^ Root Scope.
  , _lastTokenInfo :: TokenInfo
  -- ^ Used to generate precise errors locations
  }


-------------------------------------------------------------------------------
-- * Object relate type classes

class Callable mm o where
  call :: PathVar -> [o] -> mm o
  directCall :: o -> [o] -> mm o

class Iterable mm o where
  mapOver :: o -> (o -> mm o) -> mm o

class Booleanable mm o where
  checkBool :: o -> mm Bool

class Showable mm o where
  showObject :: o -> mm (Doc ())

-- | Creates reference to address specified
class GetRef mm o where
  mkRef :: PathVar -> mm (o, Address)

class Redirection mm where
  follow' :: Address -> mm Address

follow :: (MemoryAccessor mm o, Redirection mm) => Address -> mm o
follow word = do
  word' <- follow' word
  unwrap <$> getVar word'

class AccessHierarchy mm o where
  access :: o -> T.Text -> mm Address

class GetInnerRefs o where
  innerRefs :: o -> [Address]

class ToObject o where
  toObject :: o -> StWorld Object

class FromObject o where
  fromObject :: Object -> StWorld o

type BasicObjectOps mm o = (Callable mm o, Iterable mm o, Booleanable mm o, Redirection mm, AccessHierarchy mm o)

data Object
  = OStr T.Text
  | OBool Bool
  | ODouble Double
  | ONum Int
  | ORegex Regex
  -- ^ Regex expression following PCRE syntax
  | OShellCommand T.Text
  -- ^ Shell command
  | OVector (V.Vector Address)
  -- ^ Sequence of objects
  | forall prog. (Runnable prog StWorld Object, Prettify (prog Object))
    => OFunc (HM.HashMap T.Text Address) [Address] ([Object] -> prog Object)
  -- ^ Lambda with possible scope/vars attached
  | OBound Address Address
  -- ^ Used to Bound methods to variables
  | OObject (Maybe Address) (HM.HashMap T.Text Address)
  -- ^ Object instance from class Address
  | ONative ([Object] -> StWorld Object)
  -- ^ Native function
  | ONativeObject Any
  -- ^ Native object. It can't be interacted by scriptflow directly, no copy or move
  -- this object. It is just a read only (Seg faults could happend it isn't handle corretly)
  -- See Unsafe.Coerce
  | ORef Address
  -- ^ Pointer reference
  | OClassDef
    { nameClass       :: T.Text
    , refClass        :: Address
    , attributesClass :: HM.HashMap T.Text Address
    }
  | ONone

-------------------------------------------------------------------------------
-- * Instruction relate type classes
class Runnable prog mm o where
  runProgram :: prog o -> mm o

-------------------------------------------------------------------------------
-- * AST relate type classes
-- | Apply a transformation into AST, it can varies internal info type of ast.
-- This transformation be able to carry out in monad typed
class Monad m => Desugar ast a m ast' b | a ast -> b, a ast -> ast' where
  transform :: ast a -> m (ast' b)

makeSuffixLenses ''PathVar
makeSuffixLenses ''Rc
makeSuffixLenses ''World
makeSuffixLenses ''ScopeInfoAST
makeSuffixLenses ''ScopeInfo
makeSuffixLenses ''Scope
