{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
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
-- import           Compiler.Prettify
import           Compiler.Utils


newtype ShellType = ShellType { unShell :: T.Text }

newtype IdName = IdName { unIdName :: Int } deriving (Show, Eq, Num, Pretty, Ord)

pattern ID :: Int -> IdName
pattern ID i = IdName i

-- | Allows generation of new names, with its ids associated
class Monad sc => Naming sc where
  newId :: T.Text -> sc PathVar
  getNewId :: sc IdName
  findAddress :: T.Text -> sc (Maybe PathVar)
  findAddress = findAddress' . return
  findAddress' :: NL.NonEmpty T.Text -> sc (Maybe PathVar)
  {-# MINIMAL newId, getNewId, findAddress' #-}

-- | Mainly used for debug propurses or type info display
class TypeName o where
  typeName :: o -> Text

newtype ScopeInfo = ScopeInfo
  { _renameInfo :: HM.HashMap T.Text PathVar
  } deriving (Show, Eq)

type ScopeM = ExceptT (ErrorInfo ScopeError) (StateT Scope IO)

-- | Defines stack scope for program
data Scope = Scope
  { _currentScope :: ScopeInfo
  -- ^ New variables declares in the current scope to be later added to
  -- stackScope
  , _stackScope   :: [ScopeInfo]
  -- ^ All above scopes
  } deriving (Show, Eq)


data ScopeInfoAST = ScopeInfoAST
  { _tokenInfo :: TokenInfo
  , _scopeInfo :: ScopeInfo
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Memory relate types

newtype Address = Address { unAddr :: Int } deriving (Show, Eq, Num, Pretty, Ord)

pattern Adr :: Int -> Address
pattern Adr i = Address i

data PathVar = PathVar
  { _ref     :: Address
  , _dynPath :: [Text]
  }
  deriving (Show, Eq)

-- data AddressVar = AddressVar
--   { _ref     :: Address
--   , _dynPath :: [Text]
--   }
--   deriving (Show, Eq)

simple :: Address -> PathVar
simple addr = PathVar addr []

instance Pretty PathVar where
  pretty (PathVar r p) =
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
  setPathVar :: PathVar -> Store mm o -> mm Address


class Deallocate mm where
  collectAddr :: Address -> mm ()
  removeLocal :: mm ()
  deleteVar :: Address -> mm Bool
  deleteUnsafe :: Address -> mm ()

idToAdr :: IdName -> Address
idToAdr (IdName i) = Adr i

adrToId :: Address -> IdName
adrToId (Address i) = IdName i

newVar :: MemoryAccessor mm Object => Store mm Object -> mm Address
newVar var = do
  addr <- idToAdr <$> getNewId
  setVar addr var
  return addr

newVarWithName :: MemoryAccessor mm Object => T.Text -> Object -> mm Address
newVarWithName nameId obj = do
  ref <- _ref <$> newId nameId
  setVar ref (pure obj)
  return ref

getVarWithName
  :: (MonadError (ErrorInfo WorldError) mm
     , GetInfo mm, MemoryAccessor mm Object)
  => T.Text -> mm (Store mm Object, Address)
getVarWithName nameId = do
  mPathVar <- findAddress nameId
  case mPathVar of
    Just pathVar -> findPathVar pathVar
    Nothing      -> throw (ScopeError (NotDefinedObject nameId))

type StWorld = StateT (World Object) (ExceptT (ErrorInfo WorldError) IO)

data Rc o = Rc
  { _refCounter :: !Int
  , _rawObj     :: !o
  } deriving (Eq, Show)

-- | Keeps all information of running program (memory, debugging info ...)
data World o = World
  { _table         :: IM.IntMap (Rc o)
  -- ^ Generic table to storage all vars/objects
  , _scope         :: Scope
  -- ^ Root Scope.
  , _lastTokenInfo :: TokenInfo
  -- ^ Used to generate precise errors locations
  , _gc            :: [Address]
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Object relate type classes

class Callable mm where
  call :: PathVar -> [Passing] -> mm Passing

class Iterable mm where
  mapOver :: Passing -> (Address -> mm ()) -> mm ()

class Booleanable mm where
  checkBool :: Passing -> mm Bool

class Showable mm o where
  showObject :: o -> mm (Doc ann)

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

-- type BasicObjectOps mm o = (Callable mm o, Iterable mm o, Booleanable mm o, Redirection mm, AccessHierarchy mm o)

data Object
  = OStr T.Text
  | OBool Bool
  | ODouble Double
  | ONum Int
  | ORegex T.Text Regex
  -- ^ Regex expression following PCRE syntax
  | OShellCommand T.Text
  -- ^ Shell command
  | OVector (V.Vector Address)
  -- ^ Sequence of objects
  | forall prog. (Runnable prog StWorld, Pretty (prog Passing))
    => OFunc [Address] [Address] ([Passing] -> prog Passing)
  -- ^ Lambda with possible scope/vars attached
  | OBound Address Address
  -- ^ Used to Bound methods to variables
  | OObject (Maybe Address) (HM.HashMap T.Text Address)
  -- ^ Object instance from class Address
  | ONative ([Passing] -> StWorld Passing)
  -- ^ Native function
  | ONativeObject Any
  -- ^ Native object. It can't be interacted by scriptflow directly, no copy or move
  -- this object. It is just a read only (Seg faults could happend it isn't handle corretly)
  -- See Unsafe.Coerce
  | ORef Address
  -- ^ Pointer reference
  | OClassDef
    { nameClass       :: T.Text
    , attributesClass :: HM.HashMap T.Text Address
    }
  | ONone

instance Eq Object where
  obj1 == obj2 = case (obj1, obj2) of
    (OClassDef{}     , OClassDef{}     ) -> True
    (ONative{}       , ONative{}       ) -> True
    (OFunc{}         , OFunc{}         ) -> True
    (OStr{}          , OStr{}          ) -> True
    (ORegex{}        , ORegex{}        ) -> True
    (OBound{}        , OBound{}        ) -> True
    (OShellCommand{} , OShellCommand{} ) -> True
    (ODouble{}       , ODouble{}       ) -> True
    (OBool{}         , OBool{}         ) -> True
    (ONum{}          , ONum{}          ) -> True
    (OVector{}       , OVector{}       ) -> True
    (ORef ref1       , ORef ref2       ) -> ref1 == ref2
    (ONone           , ONone           ) -> True
    (OObject{}       , OObject{}       ) -> True
    (ONativeObject{} , ONativeObject{} ) -> True
    _                                    -> False

instance Show Object where
  show obj = case obj of
    OClassDef{}     -> "ClassDef"
    ONative{}       -> "NativeFunction"
    OFunc{}         -> "Function"
    OStr{}          -> "Str"
    ORegex{}        -> "Regex"
    OBound{}        -> "BoundObject"
    OShellCommand{} -> "ShellCommand"
    ODouble{}       -> "Double"
    OBool{}         -> "Bool"
    ONum{}          -> "Num"
    OVector{}       -> "Vector"
    ORef ref        -> "Ref " ++ show ref
    ONone           -> "None"
    OObject{}       -> "Object"
    ONativeObject{} -> "NativeObject"


data Passing
  = ByVal Object
  | ByRef Address

instance Pretty Passing where
  pretty (ByVal obj) = "ByVal" <+> pretty (show obj)
  pretty (ByRef ref) = "ByRef" <+> pretty ref

-------------------------------------------------------------------------------
-- * Instruction relate type classes
class Runnable prog mm where
  runProgram :: prog Passing -> mm Passing

-------------------------------------------------------------------------------
-- * AST relate type classes
-- | Apply a transformation into AST, it can varies internal info type of ast.
-- This transformation be able to carry out in monad typed
class Monad m => Desugar ast a m ast' b | a ast -> b, a ast -> ast' where
  transform :: ast a -> m (ast' b)

-- * Lenses generated
makeSuffixLenses ''PathVar
makeSuffixLenses ''Rc
makeSuffixLenses ''World
makeSuffixLenses ''ScopeInfoAST
makeSuffixLenses ''ScopeInfo
makeSuffixLenses ''Scope
