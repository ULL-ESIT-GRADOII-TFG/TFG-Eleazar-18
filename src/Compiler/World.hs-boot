module Compiler.World where

import           Control.Monad.Except
import           Control.Monad.State.Strict

--import {-# SOURCE #-} Compiler.Object
import Compiler.Error
import {-# SOURCE #-} Compiler.Types

data World o

type StWorld = StateT (World Object) (ExceptT (ErrorInfo WorldError) IO)
