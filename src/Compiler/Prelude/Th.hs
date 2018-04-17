{-# LANGUAGE TemplateHaskell #-}
module Compiler.Prelude.Th where

import           Control.Monad
--import           Control.Monad.Trans.Free
import           Language.Haskell.TH

import           Compiler.Object.Types
--import Compiler.World.Types
--import Compiler.Instruction.Types

import           Compiler.Prelude.Types


{-
normalizePure'
    :: (ToObject o, FromObject a, FromObject b)
    => (a -> b -> o)
    -> [Object]
    -> FreeT Instruction StWorld Object
normalizePure' fun = normalize (\a b -> toObject $ fun a b)
-}

normalizeArity :: Int -> Q Exp
normalizeArity n = do
    vs <- replicateM n (newName "vs")
    --tvs <- replicateM n (newName "a")
    --out <- newName "o"
    [| \f -> normalize
        $(lamE (map varP vs)
            [|
              toObject $(
                    foldl appE
                        [| f |]
                        (map varE vs))
            |])
      -- :: (ToObject $(varT out), $()) => ($(foldl appT  (varT out)  (map (appT arrowT) (map varT tvs)))) -> [Object] -> FreeT Instruction StWorld Object
     |]
