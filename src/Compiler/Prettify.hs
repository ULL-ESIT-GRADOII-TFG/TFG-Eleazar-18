module Compiler.Prettify where

import           Text.PrettyPrint


class Prettify a where
  prettify :: a -> Int -> Doc
