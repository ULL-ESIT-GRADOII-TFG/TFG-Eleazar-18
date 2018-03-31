{-# LANGUAGE TemplateHaskell #-}
module Compiler.Prelude.Th where

{-
import Control.Applicative
import Control.Monad
import Language.Haskell.TH

--import Compiler.Prelude.Types

-- take function
-- include :: () -> Q Exp

include :: Int -> Q Exp
include n = do
    vs <- replicateM n (newName "vs")
    [| \f ->
        $(lamE (map varP vs)
            [| getZipList $
                $(foldl
                    (\a b -> [| $a <*> $b |])
                    [| pure f |]
                    (map (\v -> [| ZipList $(varE v) |]) vs))
            |])
     |]
-}