module Compiler.Prettify where

import           Data.Text.Prettyprint.Doc


class Prettify a where
  -- | Item to prettify with level of verbosity
  prettify :: a -> Int -> Doc ()
