{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compiler.Prelude.Th where

import           Control.Monad
import qualified Data.Text                  as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Compiler.Error
import           Compiler.Types


instance Lift T.Text where
  lift t = [| T.pack $(lift $ T.unpack t) |]

methodsTh :: [(T.Text, ExpQ)] -> DecsQ
methodsTh methds =
  [d|
    methods :: [(T.Text, [Object] -> StWorld Object)]
    methods = $(listE (map (\(name, expr) -> [| (name, $(normalize expr)) |]) methds))
    |]


fn :: T.Text -> ExpQ -> (T.Text, ExpQ)
fn name expr = (name, expr)

normalize :: ExpQ -> Q Exp
normalize expr = do
  expr' <- expr
  (func, ty) <- splitSig expr'
  let argsTypes = getArgsType ty
  let numArgs = length argsTypes
  args <- replicateM numArgs (newName "args")
  argsObj <- replicateM numArgs (newName "argsObj")
  objs <- newName "objs"

  lamE [varP objs]
    [| do
      let expectedArgs = numArgs
          givenArgs    = length $(varE objs)
      case compare givenArgs expectedArgs of
        LT -> throw $ NumArgsMissmatch expectedArgs givenArgs
        GT -> throw $ NumArgsMissmatch expectedArgs givenArgs
        EQ ->
          $(doE $
            [ letS [ valD (listP (map varP args)) (normalB (varE objs)) []]]
            ++
            map (\(arg, argObj, ty') ->
              bindS (varP argObj)
                (appE [| fromObject :: $(forallT [] (return $ getForallConstraint ty) [t| Object -> StWorld $(return ty') |])  |] (varE arg))
            ) (zip3 args argsObj argsTypes)
            ++
            [ noBindS (appE
                [| toObject |]-- :: $(return $ getReturnType ty) -> StWorld Object |]
                (foldl
                  appE
                  (return func)
                  (map varE argsObj)
                )
            )
            ]
          )
      |]


getArgsType :: Type -> [Type]
getArgsType (ForallT _ _ t)  = getArgsType t
getArgsType (AppT ArrowT t)  = [t]
getArgsType (AppT app1 app2) = getArgsType app1 ++ getArgsType app2
getArgsType _                = []


getReturnType :: Type -> Type
getReturnType (ForallT _ _ t) = getReturnType t
getReturnType (AppT ArrowT t) = t
getReturnType (AppT _ app2)   = getReturnType app2
getReturnType t               = t

splitSig :: Exp -> Q (Exp, Type)
splitSig (SigE ex ty) = return (ex, ty)
splitSig _ = fail "Expected an expression surrounded by a signature"

getForallConstraint :: Type -> Cxt
getForallConstraint (ForallT _tyVar context _) = context
getForallConstraint _                          = []


-- firstArgType :: Type -> Type
-- firstArgType (AppT _con sub) = sub
-- firstArgType _               = error "Puff bad logic getting subtype"
