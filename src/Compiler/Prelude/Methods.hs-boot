module Compiler.Prelude.Methods where

import qualified Data.Text                  as T

import           Compiler.Types


getMethods :: Object -> T.Text -> Maybe ([Object] -> Prog)

