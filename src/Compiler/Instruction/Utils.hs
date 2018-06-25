{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Instruction.Utils where

import           Control.Monad.Trans.Free
import qualified Data.Text                as T
import           Text.PrettyPrint

import           Compiler.Types

pAddr :: AddressRef -> String
pAddr (AddressRef addr vals) =
  let path = if not $ null vals then "-" ++ T.unpack (T.intercalate "." vals) else ""
  in "#" ++ show addr ++ path


pprint :: FreeT Instruction StWorld Object -> StWorld Doc
pprint instrs = (\f -> iterT f (fmap (const mempty) instrs)) $ \case
  CallCommand _ idFun args next -> do
    let callDoc = text "Call " <> text (pAddr idFun) <> text " With: " <> text (show args)
    docs <- next ONone
    return $ callDoc $$ docs

  Assign _ idObj accObject next -> do
    let assignPP = text "Assign " <> text (pAddr idObj) <> text " " <> text (show accObject)
    docs <- next ONone
    return $ assignPP $$ docs

  DropVar _ idObj next -> do
    let dropVarPP = text "Drop " <> text (pAddr idObj)
    docs <- next
    return $ dropVarPP $$ docs

  Loop _ accObject prog next -> do
    let loopPP = text "Loop " <> text (show accObject)
    bodyLoop <- pprint $ prog ONone
    docs <- next
    return $ loopPP $$ nest 2 bodyLoop $$ docs

  Cond _ objectCond trueNext falseNext next -> do
    let condPP = text "Cond " <> text (show objectCond)
    trueBody <- pprint trueNext
    falseBody <- pprint falseNext
    docs <- next ONone
    return $ condPP $$
      text "True Case:" $$
      nest 2 trueBody $$
      text "False Case:" $$
      nest 2 falseBody $$
      docs

  GetVal _ idObj next -> do
    let getValPP = text "GetVal " <> text (pAddr idObj)
    docs <- next ONone
    return $ getValPP $$ docs
