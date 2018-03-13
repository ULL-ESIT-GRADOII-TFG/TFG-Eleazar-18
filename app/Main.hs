module Main where

import           Compiler.Core
import           Data.Monoid
import           Options.Applicative

data ArgsConfig = Interpreter

main :: IO ()
main = execParser opts >>= runAppWithConfig
  where
    opts = info (helper <*> pure Interpreter)
           (fullDesc
            <> progDesc "Interpreter and runtime execution of gitscript files"
            <> footer "Any bug should be reported to https://github.com/ULL-ESIT-GRADOII-TFG/TFG-Eleazar-17/issues")


runAppWithConfig :: ArgsConfig -> IO ()
runAppWithConfig Interpreter = start
