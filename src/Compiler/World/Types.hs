{-# LANGUAGE TemplateHaskell #-}
module Compiler.World.Types where


data WorldError
  = NotFoundObject
  | NotIterable
  | NotCallable
  | NumArgsMissmatch
  | NotImplicitConversion
  deriving Show
