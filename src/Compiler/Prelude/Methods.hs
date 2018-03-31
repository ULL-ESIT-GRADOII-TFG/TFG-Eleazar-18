{-# LANGUAGE OverloadedStrings #-}
module Compiler.Prelude.Methods where

import           Control.Monad.Trans.Free
import           Control.Monad.IO.Class
import qualified Data.Text as T

import Compiler.Prelude.Types
import Compiler.Object.Types
import Compiler.World.Types
import Compiler.Instruction.Types


baseObjects :: [(T.Text, Object)]
baseObjects =
    [ ("print", ONative (normalize printObj))
    , ("not"  , ONative (normalizePure not))
    ]

printObj :: Object -> FreeT Instruction StWorld VarAccessor
printObj obj = liftIO $ print obj >> return (Raw ONone)

normalizePure
    :: (ToObject o, FromObject a)
    => (a -> o)
    -> [Object]
    -> FreeT Instruction StWorld VarAccessor
normalizePure fun = normalize (toObject . fun)



