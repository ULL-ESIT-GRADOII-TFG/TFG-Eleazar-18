{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Instruction.Methods where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State.Strict
import qualified Data.Text.Lazy                   as LT
import qualified Data.Vector                      as V
import qualified Data.Map                         as M
import           Formatting
import           Lens.Micro.Platform

import           Compiler.Ast
import           Compiler.Instruction.Types
import           Compiler.Object.Methods
import           Compiler.Object.Types
import           Compiler.World.Methods
import           Compiler.World.Types


-- | Transform AST to a simplified intermediate language, more related to
-- memory management
astToInstructions
  :: (Monad m) => ExpressionG Identity last AddressRef -> FreeT Instruction m Object
astToInstructions expr = case expr of
  FunDecl args prog _info ->
    return $ OFunc mempty (map (\(AddressRef ref' _) -> ref') args) (astToInstructions prog)

  VarDecl ref' exprValue _info -> do
    value <- astToInstructions exprValue
    runIdentity ref' =: value
    return ONone

  SeqExpr exprs _info ->
    foldM (\_ expr' -> astToInstructions expr') ONone exprs

  If condExpr prog _info -> do
    value <- astToInstructions condExpr
    cond value (astToInstructions prog) (return ONone)

  IfElse condExpr trueProg falseProg _info -> do
    value <- astToInstructions condExpr
    cond value (astToInstructions trueProg) (astToInstructions falseProg)

  For ref' iterExpr prog _info -> do
    value <- astToInstructions iterExpr
    loop value (\val -> ref' =: val >> astToInstructions prog)
    return ONone

  Apply ref' argsExpr _info -> do
    values' <- mapM astToInstructions argsExpr
    callCommand (runIdentity ref') values'

  Identifier ref' _info ->
    getVal (runIdentity ref')

  Factor atom _info -> fromAST atom

-- | Transform literal data from AST to an object
fromAST :: (Monad m) => AtomG Identity a AddressRef -> FreeT Instruction m Object
fromAST atom =
  case atom of
    ANum num          -> return $ ONum num
    AStr str          -> return $ OStr str
    ADecimal double   -> return $ ODouble double
    ARegex reg        -> return $ ORegex reg
    AShellCommand cmd -> return $ OShellCommand cmd
    ABool bool        -> return $ OBool bool
    AVector items     -> mapM astToInstructions items >>= return . OVector . V.fromList
    ADic items        -> mapM (\(key, expr) -> astToInstructions expr >>= return . (key,)) items >>= return . ODic . M.fromList

-- | Execute a sequence of instructions
runProgram :: FreeT Instruction StWorld Object -> StWorld Object
runProgram = iterT $ \case
  -- Find into world function and correspondent objects
  CallCommand idFun args next -> do
    -- TODO:Es necesario unir estos dos para poder hacer la llamada a un metodo del objeto
    retObj  <- callObject idFun args
    next retObj

  Assign idObj accObject next -> do
    -- object <- getObject accObject
    addObject idObj accObject
    next

  DropVar idObj next -> do
    dropVarWorld idObj
    next

  Loop accObject prog next -> do
    -- oIter <- getObject accObject
    _     <- mapObj accObject (runProgram . prog)
    next

  Cond objectCond trueNext falseNext next -> do
    bool <-  checkBool objectCond
    if bool
      then runProgram trueNext >>= next
      else runProgram falseNext >>= next

  GetVal idObj next -> do
    ref' <- findObject idObj
    next ref'

  End               -> return ONone

newFakeRef :: StPrint Word
newFakeRef = do
  fakeId += 1
  use fakeId

linePP :: LT.Text -> StPrint ()
linePP txt =
  -- Fix Indentation
  generate %= (`LT.append` txt)

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
    linePP (format ("Call #" % shown % " With: " % shown) idFun args)
    refId <- newFakeRef -- show id where is generate
    next $ ORef refId

  Assign idObj accObject next -> do
    linePP (format ("#" % shown % " = " % shown) idObj accObject)
    next

  DropVar idObj next -> do
    linePP (format ("Drop #" % shown) idObj)
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
    next $ ORef refId

  GetVal idObj next -> do
    linePP (format ("Loop " % shown) idObj)
    refId <- newFakeRef -- show id where is generate
    next $ ORef refId

  End -> linePP "End"

-------------------------------------------------------------------------------
-- * Utils
-------------------------------------------------------------------------------
callCommand
  :: (MonadFree Instruction m) => AddressRef -> [Object] -> m Object
callCommand nameId objs = liftF (CallCommand nameId objs id)

(=:) :: (MonadFree Instruction m) => AddressRef -> Object -> m ()
nameId =: obj = liftF (Assign nameId obj ())

dropVar :: (MonadFree Instruction m) => AddressRef -> m ()
dropVar r = liftF (DropVar r ())

loop
  :: (MonadFree Instruction m)
  => Object
  -> (Object -> FreeT Instruction StWorld Object)
  -> m ()
loop obj prog = liftF (Loop obj prog ())

cond
  :: (MonadFree Instruction m)
  => Object
  -> FreeT Instruction StWorld Object
  -> FreeT Instruction StWorld Object
  -> m Object
cond obj true false = liftF (Cond obj true false id)

getVal :: (MonadFree Instruction m) => AddressRef -> m Object
getVal obj = liftF (GetVal obj id)

end :: (MonadFree Instruction m) => m a
end = liftF End