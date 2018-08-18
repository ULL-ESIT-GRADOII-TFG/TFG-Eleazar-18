{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Compiler.Types where

import           Control.Monad.Except
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

import           Compiler.Error
import           Compiler.Prettify
import           Compiler.Utils


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

-- | Allows generation of new names, with its ids associated
class Monad sc => Naming sc where
  newId :: T.Text -> sc PathVar
  getNewId :: sc Address
  findAddress :: T.Text -> sc PathVar

-- | Mainly used for debug propurses or type info display
class TypeName o where
  typeName :: o -> Text

class Applicative r => Wrapper r where
  wrap :: a -> r a
  wrap = pure
  unwrap :: r a -> a

class (Naming mm, Wrapper (Store mm), MonadError (ErrorInfo WorldError) mm, GetInfo mm, MonadIO mm)
    => MemoryManagement mm where
  type Store mm :: * -> *
  type RawObj mm :: *

  getVar :: Address -> mm (Store mm (RawObj mm))
  setVar :: Address -> Store mm (RawObj mm)-> mm ()
  deleteVar :: Address -> mm Bool
  deleteUnsafe :: Address -> mm ()
  -- | Look into memory to find the final object pointed, and returns also its address
  -- Fails in case of not found the object `NotFoundObject`
  findPathVar :: PathVar -> mm (Store mm (RawObj mm), Address)
  setPathVar :: PathVar -> Store mm (RawObj mm) -> mm ()

newVar :: MemoryManagement mm => Store mm (RawObj mm) -> mm Address
newVar var = do
  addr <- getNewId
  setVar addr var
  return addr

newVarWithName :: MemoryManagement mm => T.Text -> RawObj mm -> mm Address
newVarWithName nameId obj = do
  ref <- _ref <$> newId nameId
  setVar ref (pure obj)
  return ref

getVarWithName :: MemoryManagement mm => T.Text -> mm (Store mm (RawObj mm))
getVarWithName nameId = fst <$> (findAddress nameId >>= findPathVar)

class (InstructionsLike mm, MemoryManagement mm, o ~ RawObj mm)
    => ObjectOperations mm o where
  call :: PathVar -> [o] -> mm o
  directCall :: RawObj mm -> [RawObj mm] -> mm (RawObj mm)
  mapOver :: RawObj mm -> (RawObj mm -> mm (RawObj mm)) -> mm (RawObj mm)
  checkBool :: RawObj mm -> mm Bool
  showObject :: RawObj mm -> mm (Doc ())
  -- | Redirect if the object is a reference to another object, it returns the last object address
  -- without be a reference. It should/could throw an exception of redirection limit.
  redirect :: Address -> mm Address
  zoom :: RawObj mm -> T.Text -> mm Address

class InstructionsLike mm where
  type Prog mm :: (* -> *) -> * -> *
  runProgram :: Prog mm mm (RawObj mm) -> mm (RawObj mm)

  showInstructions :: Prog mm mm (RawObj mm) -> mm (Doc ())

-- type TkState st m = StateT (TkSt st) m

-- | Intermediate data type to keeps tracks of current TokenInfo
-- data TkSt a = TkSt
--   { _innerState       :: a
--   , _currentTokenInfo :: TokenInfo
--   -- ^ Used to generate precise errors locations
--   } deriving Show


-- instance Default a => Default (TkSt a) where
--   def = TkSt def def

-- instance Monad m => GetInfo (StateT (TkSt st) m) where
--   getInfo = _currentTokenInfo <$> get


-- | Apply a transformation into AST, it can varies internal info type of ast.
-- This transformation be able to carry out in monad typed
class Monad m => Desugar ast a m ast' b | a ast -> b, a ast -> ast' where
  transform :: ast a -> m (ast' b)


makeSuffixLenses ''PathVar
--makeSuffixLenses ''TkSt
