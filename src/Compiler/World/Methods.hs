module Compiler.World.Methods where

import Lens.Micro.Platform
import qualified Data.IntMap                as IM

import Compiler.World.Types
import Compiler.Object.Types
import Compiler.Instruction.Types


addObject :: Word -> Object -> StWorld ()
addObject ref obj = table %= IM.insert (fromIntegral ref) (Var 0 obj)

findVar :: Word -> StWorld Object
findVar ref = do
  objects <- use table
  case IM.lookup (fromIntegral ref) objects of
    Just var -> return $ var^.rawVar
    Nothing -> return ONone

dropVarWorld :: Word -> StWorld ()
dropVarWorld = undefined

getObject :: VarAccessor -> StWorld Object
getObject (Raw obj) = return obj
getObject (Ref word) = findVar word
