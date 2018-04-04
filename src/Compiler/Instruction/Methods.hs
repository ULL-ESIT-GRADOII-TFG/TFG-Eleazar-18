{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Instruction.Methods where

import           Control.Monad
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State.Strict
import qualified Data.Text.Lazy as T
import           Lens.Micro.Platform
import           Formatting

import Compiler.Ast
import Compiler.Instruction.Types
import Compiler.World.Types
import Compiler.World.Methods
import Compiler.Object.Types
import Compiler.Object.Methods


callCommand
  :: (MonadFree Instruction m) => Word -> [VarAccessor] -> m VarAccessor
callCommand nameId objs = liftF (CallCommand nameId objs id)

(=:) :: (MonadFree Instruction m) => Word -> VarAccessor -> m ()
nameId =: obj = liftF (Assign nameId obj ())

dropVar :: (MonadFree Instruction m) => Word -> m ()
dropVar ref = liftF (DropVar ref ())

loop
  :: (MonadFree Instruction m)
  => VarAccessor
  -> (VarAccessor -> FreeT Instruction StWorld VarAccessor)
  -> m ()
loop obj prog = liftF (Loop obj prog ())

cond
  :: (MonadFree Instruction m)
  => VarAccessor
  -> FreeT Instruction StWorld VarAccessor
  -> FreeT Instruction StWorld VarAccessor
  -> m VarAccessor
cond obj true false = liftF (Cond obj true false id)

getVal :: (MonadFree Instruction m) => Word -> m VarAccessor
getVal obj = liftF (GetVal obj id)

end :: (MonadFree Instruction m) => m a
end = liftF End

getRefFromAccessor :: (Monad m) => AccessorG a Word -> FreeT Instruction m Word
getRefFromAccessor (Simple word _) = return word
getRefFromAccessor _ = error "Bracket or Dot not handle on instruction"

-- | Transform AST to a simplified intermediate language, more related to
-- memory management
astToInstructions
  :: (Monad m) => ExpressionG last Word -> FreeT Instruction m VarAccessor
astToInstructions expr = case expr of
  FunDecl args prog _info ->
    return . Raw $ OFunc mempty args (astToInstructions prog)

  VarDecl acc exprValue _info -> do
    value <- astToInstructions exprValue
    ref <- getRefFromAccessor acc
    ref =: value
    return . Raw $ ONone

  SeqExpr exprs _info ->
    foldM (\_ expr' -> astToInstructions expr') (Raw ONone) exprs

  If condExpr prog _info -> do
    value <- astToInstructions condExpr
    cond value (astToInstructions prog) (return $ Raw ONone)

  IfElse condExpr trueProg falseProg _info -> do
    value <- astToInstructions condExpr
    cond value (astToInstructions trueProg) (astToInstructions falseProg)

  For ref iterExpr prog _info -> do
    value <- astToInstructions iterExpr
    loop value (\val -> ref =: val >> astToInstructions prog)
    return . Raw $ ONone

  Apply acc argsExpr _info -> do
    values <- mapM astToInstructions argsExpr
    ref <- getRefFromAccessor acc
    callCommand ref values

  Identifier acc  _info -> do
    ref <- getRefFromAccessor acc
    getVal ref

  Factor     atom _info -> return . Raw $ fromAST atom


-- | TODO: Change type it should be possible make errors in this phase
runProgram :: FreeT Instruction StWorld VarAccessor -> StWorld VarAccessor
runProgram = iterT $ \case
  -- Find into world function and correspondent objects
  CallCommand idFun args next -> do
    obj     <- findVar idFun
    argsObj <- mapM getObject args
    retObj  <- callObject obj argsObj
    next retObj

  Assign idObj accObject next -> do
    object <- getObject accObject
    addObject idObj object
    next

  DropVar idObj next -> do
    dropVarWorld idObj
    next

  Loop accObject prog next -> do
    oIter <- getObject accObject
    _     <- mapObj oIter (runProgram . prog . Raw)
    next

  Cond objectCond trueNext falseNext next -> do
    boolean <- getObject objectCond
    if checkBool boolean
      then runProgram trueNext >>= next
      else runProgram falseNext >>= next

  GetVal idObj next -> next $ Ref idObj

  End               -> return $ Raw ONone

newFakeRef :: StPrint Word
newFakeRef = do
  fakeId += 1
  use fakeId

linePP :: T.Text -> StPrint ()
linePP txt =
  -- Fix Indentation
  generate %= (`T.append` txt)

newLevel :: StPrint PPrint
newLevel = do
  level += 1
  get

unnestLevel :: PPrint -> StPrint ()
unnestLevel = undefined

-- TODO: Make pretty printer
pprint :: FreeT Instruction StPrint () -> StPrint ()
pprint = iterT $ \case
  CallCommand idFun args next -> do
    linePP (format ("Call #" % int % " With: " % shown) idFun args)
    refId <- newFakeRef -- show id where is generate
    next $ Ref refId

  Assign idObj accObject next -> do
    linePP (format ("#" % int % " = " % shown) idObj accObject)
    next

  DropVar idObj next -> do
    linePP (format ("Drop #" % int) idObj)
    next

  Loop accObject _prog next -> do
    linePP (format ("Loop " % shown) accObject)
    _level <- newLevel
    --pprint prog
    next

  Cond objectCond _trueNext _falseNext next -> do
    linePP (format ("Cond " % shown) objectCond)
    _level <- newLevel
    --pprint prog
    refId  <- newFakeRef -- show id where is generate
    next $ Ref refId

  GetVal idObj next -> do
    linePP (format ("Loop " % int) idObj)
    refId <- newFakeRef -- show id where is generate
    next $ Ref refId

  End -> linePP "End"
