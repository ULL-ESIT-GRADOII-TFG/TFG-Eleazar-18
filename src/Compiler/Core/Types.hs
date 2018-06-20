module Compiler.Core.Types where


-- | Arguments for executable
data ArgsConfig
  = Interpreter
    { inputFile  :: Maybe String
    , configPath :: Maybe String
    , startREPL  :: Bool
    , verbose    :: Int
    -- ^ Set a verbose level 0,1,2,3. See `prettify`
    }
  | Version
