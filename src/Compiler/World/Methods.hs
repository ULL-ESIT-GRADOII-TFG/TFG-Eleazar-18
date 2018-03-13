module Compiler.World.Methods where

-- import Lens.Micro.Platform

import Compiler.World.Types
import Compiler.Object.Types
import Compiler.Instruction.Types


addObject :: Word -> Object -> StWorld ()
addObject _ _ = undefined

findVar :: Word -> StWorld Object
findVar = undefined

dropVarWorld :: Word -> StWorld ()
dropVarWorld = undefined

getObject :: VarAccessor -> StWorld Object
getObject (Raw obj) = return obj
getObject (Ref word) = findVar word
