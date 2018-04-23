module Compiler.Prelude.Methods where

import qualified Data.Text                  as T

import           Compiler.Instruction.Types
import           Compiler.Object.Types

getMethods :: Object -> T.Text -> Maybe ([Object] -> Prog)

