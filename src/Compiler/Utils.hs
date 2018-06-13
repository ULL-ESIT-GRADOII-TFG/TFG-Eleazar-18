{-# LANGUAGE TemplateHaskell #-}
module Compiler.Utils where

import           Data.Char
import           Language.Haskell.TH
import           Lens.Micro.Platform


-- | Add 'A' suffix into lenses generated
makeSuffixLenses :: Name -> DecsQ
makeSuffixLenses = makeLensesWith (
  lensRules & lensField .~ \_ _ n ->
         case nameBase n of
           '_':x:xs -> [TopName (mkName (toLower x:xs ++ "A"))]
           _        -> [])
