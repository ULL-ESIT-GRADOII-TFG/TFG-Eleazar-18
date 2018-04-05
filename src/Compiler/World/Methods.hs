{-# LANGUAGE LambdaCase #-}
module Compiler.World.Methods where

import Lens.Micro.Platform
import qualified Data.IntMap                as IM

import Compiler.World.Types
import Compiler.Object.Types
import Compiler.Instruction.Types


addObject :: [Word] -> Object -> StWorld ()
addObject rfs obj = do
  table' <- use table
  mapM_ (table .=) (addObjectOnTable table' rfs)
  where
    addObjectOnTable :: IM.IntMap Var -> [Word] -> Maybe (IM.IntMap Var)
    addObjectOnTable _ [] = Nothing
    addObjectOnTable tbl [r] = Just $ IM.insert (fromIntegral r) (Var 0 obj) tbl
    addObjectOnTable tbl (r:rfs) =
      IM.lookup (fromIntegral r) tbl >>=
      (\case
        var@Var{} ->
            Nothing
        setVar@SetVar{} ->
            addObjectOnTable (_vars setVar) rfs
        )

findVar :: [Word] -> StWorld Object
findVar ws = do
  objects <- use table
  return $ lookupThroughRefs objects ws
 where
  lookupThroughRefs :: IM.IntMap Var -> [Word] -> Object
  lookupThroughRefs tbl [] = ONone
  lookupThroughRefs tbl (w:wss) =
    case IM.lookup (fromIntegral w) tbl of
      Just var@Var{} ->
        if null wss then _rawVar var else ONone
      Just setVar@SetVar{} -> if null wss
        then _rawMainVar setVar
        else lookupThroughRefs (setVar ^. vars) wss
      Nothing -> ONone

-- | TODO:
dropVarWorld :: [Word] -> StWorld ()
dropVarWorld = undefined

getObject :: Object -> StWorld Object
getObject obj = return obj
getObject (ORef word) = findVar word
