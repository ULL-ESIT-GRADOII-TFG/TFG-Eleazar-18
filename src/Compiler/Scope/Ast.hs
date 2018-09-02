{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies              #-}
module Compiler.Scope.Ast where

import Compiler.Ast
import Compiler.Parser.Types
import Compiler.Types

data Rn
type ExprRn = Expression Rn

type instance XName Rn = IdName
type instance XAccessor Rn = PathVar

type instance XFunExpr Rn = Empty
type instance XVarExpr Rn = Empty
type instance XSeqExpr Rn = Empty
type instance XMkScope Rn = ScopeInfo
type instance XIf Rn = Empty
type instance XIfElse Rn = Empty
type instance XFor Rn = Empty
type instance XApply Rn = Empty
type instance XIdentifier Rn = Empty
type instance XFactor Rn = Empty

pattern RnFunExpr :: [IdName] -> ExprRn -> Loc -> ExprRn
pattern RnFunExpr acc expr loc <- FunExpr _ acc expr loc
  where RnFunExpr acc expr loc = FunExpr absurb acc expr loc

pattern RnVarExpr :: PathVar -> ExprRn -> Loc -> ExprRn
pattern RnVarExpr acc expr loc <- VarExpr _ acc expr loc
  where RnVarExpr acc expr loc = VarExpr absurb acc expr loc

pattern RnSeqExpr :: [ExprRn] -> Loc -> ExprRn
pattern RnSeqExpr exprs loc <- SeqExpr _ exprs loc
  where RnSeqExpr exprs loc = SeqExpr absurb exprs loc

pattern RnMkScope :: ScopeInfo -> [ExprRn] -> Loc -> ExprRn
pattern RnMkScope scopeInfo exprs loc = MkScope scopeInfo exprs loc

pattern RnIf :: ExprRn -> ExprRn -> Loc -> ExprRn
pattern RnIf expr trueExpr loc <- If _ expr trueExpr loc
  where RnIf expr trueExpr loc = If absurb expr trueExpr loc

pattern RnIfElse :: ExprRn -> ExprRn -> ExprRn -> Loc -> ExprRn
pattern RnIfElse expr trueExpr falseExpr loc <- IfElse _ expr trueExpr falseExpr loc
  where RnIfElse expr trueExpr falseExpr loc = IfElse absurb expr trueExpr falseExpr loc

pattern RnFor :: IdName -> ExprRn -> ExprRn -> Loc -> ExprRn
pattern RnFor name iterExpr bodyExpr loc <- For _ name iterExpr bodyExpr loc
  where RnFor name iterExpr bodyExpr loc = For absurb name iterExpr bodyExpr loc

pattern RnApply :: PathVar -> [ExprRn] -> Loc -> ExprRn
pattern RnApply acc exprs loc <- Apply _ acc exprs loc
  where RnApply acc exprs loc = Apply absurb acc exprs loc

pattern RnIdentifier :: PathVar -> Loc -> ExprRn
pattern RnIdentifier acc loc <- Identifier _ acc loc
  where RnIdentifier acc loc = Identifier absurb acc loc

pattern RnFactor :: Atom Rn -> Loc -> ExprRn
pattern RnFactor atom loc <- Factor _ atom loc
  where RnFactor atom loc = Factor absurb atom loc
