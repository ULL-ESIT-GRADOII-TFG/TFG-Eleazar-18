{-# LANGUAGE TemplateHaskell #-}
module Compiler.Prelude.Th where

import           Control.Monad
import           Language.Haskell.TH

import           Compiler.Error
import           Compiler.Object
import           Compiler.Types


normalize :: ExpQ -> (TypeQ -> TypeQ) -> Q Exp
normalize expr ty = do
    mm' <- newName "mm"
    let mm = varT mm'
    ty' <- ty mm
    let argsTypes = getArgsType ty'
    let numArgs = length argsTypes
    args <- replicateM numArgs (newName "args")
    argsObj <- replicateM numArgs (newName "argsObj")
    froms <- replicateM numArgs (newName "from")
    objs <- newName "objs"

    let funType =
          forallT []
          (cxt
            [ [t| MemoryManagement $(mm)|]
            , [t| RawObj $(mm) ~ Object $(mm) |]
            ]
          )
          (foldl
            (\accTy argTy ->
              [t| (Object $(mm) -> $(mm) $(return argTy)) -> $(accTy) |]
            )
            [t| [Object $(mm)] -> $(mm) (Object $(mm)) |]
            (reverse argsTypes)
          )

    [|
      $(foldl
        appE
        (sigE (lamE (map varP froms ++ [varP objs])
          [| do
            let expectedArgs = numArgs
                givenArgs    = length $(varE objs)
            case compare givenArgs expectedArgs of
              LT -> throw $ NumArgsMissmatch expectedArgs givenArgs
              GT -> throw $ NumArgsMissmatch expectedArgs givenArgs
              EQ ->
                $(doE
                  ([letS [ valD (listP (map varP args)) (normalB (varE objs)) []]]
                  ++
                    map (\(arg, argObj, from) ->
                      bindS (varP argObj)
                        (appE
                          (varE from)
                          (varE arg)
                        )
                      ) (zip3 args argsObj froms)
                  ++ [
                    noBindS (appE
                      [| toObject |]
                      (foldl
                        appE
                        expr
                        (map varE argsObj)
                      )
                    )]
                  )
                )
          |]
        ) funType
        )
        (replicate numArgs [| fromObject |])
      )
     |]

getArgsType :: Type -> [Type]
getArgsType (ForallT _ _ t)  = getArgsType t
getArgsType (AppT ArrowT t)  = [t]
getArgsType (AppT app1 app2) = getArgsType app1 ++ getArgsType app2
getArgsType _                = []


-- getReturnType :: Type -> Type
-- getReturnType (ForallT _ _ t) = getReturnType t
-- getReturnType (AppT ArrowT t) = t
-- getReturnType (AppT _ app2)   = getReturnType app2
-- getReturnType t               = t

-- firstArgType :: Type -> Type
-- firstArgType (AppT _con sub) = sub
-- firstArgType _               = error "Puff bad logic getting subtype"
