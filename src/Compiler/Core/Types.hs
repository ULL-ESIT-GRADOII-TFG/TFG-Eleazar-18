module Compiler.Core.Types where


data ArgsConfig
  = Interpreter
    { inputFile :: Maybe String
    , configPath :: Maybe String
    , startREPL :: Bool
    }
  | Version

