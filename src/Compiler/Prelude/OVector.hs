{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Compiler.Prelude.OVector where

import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Compiler.Object
import           Compiler.Prelude.Th
import           Compiler.Types


methods
  :: (MemoryManagement mm, RawObj mm ~ Object mm)
  => [(T.Text, [Object mm] -> mm (Object mm))]
methods =
  [ ( "length"
    , $(normalize [| V.length |] (\ty -> [t| V.Vector (Object $ty) -> Int |])))
  , ( "null"
    , $(normalize [| V.null |] (\ty -> [t| V.Vector (Object $ty) -> Bool |])))
  , ( "head"
    , $(normalize [| V.head |] (\ty -> [t| V.Vector (Object $ty) -> Object $ty |])))
  , ( "last"
    , $(normalize [| V.last |] (\ty -> [t| V.Vector (Object $ty) -> Object $ty |])))
  , ( "slice"
    , $(normalize [| \v a b -> V.slice a b v |]
        (\ty -> [t| V.Vector (Object $ty) -> Int -> Int -> V.Vector (Object $ty) |])))
  , ( "take"
    , $(normalize [| flip V.take |] (\ty -> [t| V.Vector (Object $ty) -> Int -> Object $ty |])))
  , ( "drop"
    , $(normalize [| flip V.drop |] (\ty -> [t| V.Vector (Object $ty) -> Int -> Object $ty |])))
  , ( "@"
    , $(normalize [| (V.!?) |] (\ty -> [t| V.Vector (Object $ty) -> Int -> Object $ty |])))
  , ( "++"
    , $(normalize [| mappend |]
        (\ty -> [t| V.Vector (Object $ty) -> V.Vector (Object $ty) -> V.Vector (Object $ty) |])))
  ]


-- test :: (MemoryManagement mm, RawObj mm ~ Object mm)
--   => (Object mm -> mm (V.Vector (Object mm)))
--   -> (Object mm -> mm Int)
--   -> [Object mm]
--   -> mm (Object mm)
-- test from from2 objs = do
--     let expectedArgs = 2
--         givenArgs    = length objs
--     case compare givenArgs expectedArgs of
--       LT -> throw $ NumArgsMissmatch expectedArgs givenArgs
--       GT -> throw $ NumArgsMissmatch expectedArgs givenArgs
--       EQ -> do
--         let [arg,arg2] = objs
--         argObj1 <- ((from arg)) -- :: mm (V.Vector (Object mm)))
--         argObj2 <- ((from2 arg2)) -- :: mm (V.Vector (Object mm)))
--         toObject $ V.take argObj2 argObj1
