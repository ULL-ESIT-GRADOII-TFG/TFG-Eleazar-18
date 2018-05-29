{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Compiler.Instruction.Methods where

import           Control.Monad
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Class
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Vector              as V
import           Formatting
import           Lens.Micro.Platform

import           Compiler.Ast
import           Compiler.Object.Methods
import           Compiler.Types
import           Compiler.World.Methods
import           Compiler.Scope.Utils


-- | Transform AST to a simplified intermediate language, more related to
-- memory management
-- TODO: Add drop variables
astToInstructions :: Expression ScopeInfoAST -> FreeT Instruction StWorld Object
astToInstructions expr = case expr of
  FunExpr args prog info -> do
    addresses <- mapM (\acc -> do
                          addr <- lift $ getAddressRef (Simple acc ()) info
                          return $ addr^.ref
                      ) args
    return $ OFunc mempty addresses (astToInstructions prog)

  VarExpr acc exprValue info -> do
    value <- astToInstructions exprValue
    addr <- lift $ getAddressRef acc info
    addr =: value
    return ONone

  SeqExpr exprs _info ->
    foldM (\_ expr' -> astToInstructions expr') ONone exprs

  MkScope exprs ->
    foldM (\_ expr' -> astToInstructions expr') ONone exprs

  If condExpr prog _info -> do
    value <- astToInstructions condExpr
    cond value (astToInstructions prog) (return ONone)

  IfElse condExpr trueProg falseProg _info -> do
    value <- astToInstructions condExpr
    cond value (astToInstructions trueProg) (astToInstructions falseProg)

  For acc iterExpr prog info -> do
    value <- astToInstructions iterExpr
    addr <- lift $ getAddressRef (Simple acc ()) info
    loop value (\val -> addr =: val >> astToInstructions prog)
    return ONone

  Apply acc argsExpr info -> do
    values' <- mapM astToInstructions argsExpr
    addr <- lift $ getAddressRef acc info
    callCommand addr values'

  Identifier acc info -> do
    addr <- lift $ getAddressRef acc info
    getVal addr

  Factor atom _info -> fromAST atom

-- | Transform literal data from AST to an object
fromAST :: Atom ScopeInfoAST -> FreeT Instruction StWorld Object
fromAST atom =
  case atom of
    ANone             -> return ONone
    ANum num          -> return $ ONum num
    AStr str          -> return $ OStr str
    ADecimal double   -> return $ ODouble double
    ARegex reg        -> return $ ORegex reg
    AShellCommand cmd -> return $ OShellCommand cmd
    ABool bool        -> return $ OBool bool
    AVector items     -> OVector . V.fromList <$> mapM astToInstructions items
    ADic items        -> ODic . M.fromList <$> mapM (\(key, expr) -> (key,) <$> astToInstructions expr) items

-- | Execute a sequence of instructions
runProgram :: FreeT Instruction StWorld Object -> StWorld Object
runProgram = iterT $ \case
  -- Find into world function and correspondent objects
  CallCommand idFun args next -> do
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

linePP :: LT.Text -> StWorld ()
linePP txt = do
  level <- use $ debugProgram._2
  debugProgram._1 %= \t -> t `mappend` LT.replicate (fromIntegral level) "  " `mappend` txt `mappend` "\n"

withLevel :: StWorld a -> StWorld ()
withLevel action = do
  (debugProgram._2) += 1
  _ <- action
  (debugProgram._2) -= 1

pAddr :: AddressRef -> String
pAddr (AddressRef addr vals) =
  let path = if not $ null vals then "-" ++ T.unpack (T.intercalate "." vals) else ""
  in "#" ++ show addr ++ path

-- TODO: Make pretty printer
pprint :: FreeT Instruction StWorld Object -> StWorld Object
pprint = iterT $ \case
  CallCommand idFun args next -> do
    linePP (format ("Call " % string % " With: " % shown) (pAddr idFun) args)
    next ONone

  Assign idObj accObject next -> do
    linePP (format ("Assign " % string % " " % shown) (pAddr idObj) accObject)
    next

  DropVar idObj next -> do
    linePP (format ("Drop " % string) (pAddr idObj))
    next

  Loop accObject prog next -> do
    linePP (format ("Loop " % shown) accObject)
    withLevel . pprint $ prog ONone
    next

  Cond objectCond trueNext falseNext next -> do
    linePP (format ("Cond " % shown) objectCond)
    withLevel $ do
      linePP "True Case:"
      withLevel $ pprint trueNext
      linePP "False Case:"
      withLevel $ pprint falseNext

    next ONone

  GetVal idObj next -> do
    linePP (format ("GetVal " % string) (pAddr idObj))
    next ONone

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
