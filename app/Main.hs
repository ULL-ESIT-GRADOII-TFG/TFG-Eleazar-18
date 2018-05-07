module Main where

import           Compiler.Core.Methods
import           Compiler.Core.Types
import           Data.Monoid
import           Options.Applicative


main :: IO ()
main = execParser opts >>= start
  where
    opts = info (argsConfig <|> version <**> helper)
           (fullDesc
            <> progDesc "Interpreter"
            <> footer "Any bug should be reported to https://github.com/ULL-ESIT-GRADOII-TFG/TFG-Eleazar-18/issues")

argsConfig :: Parser ArgsConfig
argsConfig = Interpreter
  <$> (Just <$> (strArgument
    ( metavar "INPUT_SCRIPT"
    <> help "Interpret script given" ))
    <|> pure Nothing)
  <*> (Just <$> (strOption
    ( long "config"
    <> short 'c'
    <> metavar "CONFIG_FILE"
    <> help "Specific a different path to config" ))
    <|> pure Nothing)
  <*> switch
    ( short 'e'
    <> long "repl"
    <> help "Start repl after load script file" )

version :: Parser ArgsConfig
version = Version <$ switch (long "version" <> short 'v' <> help "Show version used")
