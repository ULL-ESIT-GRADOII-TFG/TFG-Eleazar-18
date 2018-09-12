{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Ast where

import           Compiler.Parser.Types
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           GHC.Exts

-- import           Compiler.Prettify
import           Compiler.Utils


data Repl
  = Command T.Text [T.Text]
  | Code [Statement Tok]

data Statement a
  = ClassSt (ClassDecl a)
  | FunSt (FunDecl a)
  | Expr (Expression a)

data ClassDecl a = ClassDecl
  { _clsName    :: T.Text
  , _clsMethods :: [FunDecl a]
  , _clsLoc     :: Loc
  }

data FunDecl ex = FunDecl
  { _funName :: T.Text
  , _funArgs :: [XName ex]
  , _funBody :: Expression ex
  , _funLoc  :: Loc
  }

-- | Generic representation of expression
data Expression ex
  = FunExpr (XFunExpr ex) [XName ex] (Expression ex) Loc
  -- ^ Create a lambda function
  --
  -- lam arg1 arg2:
  --   expr
  --
  | VarExpr (XVarExpr ex) (XAccessor ex) (Expression ex) Loc
  -- ^ Creates a new variable
  --
  -- var1 = expr
  -- obj1.acc1 = expr
  --
  | SeqExpr (XSeqExpr ex) [Expression ex] Loc
  -- ^ Group up expression without use a new scope
  | MkScope (XMkScope ex) [Expression ex] Loc
  -- ^ Creates a scope
  | If (XIf ex) (Expression ex) (Expression ex) Loc
  -- ^ Simple if
  --
  -- if expr_boolean:
  --   expr
  --
  | IfElse (XIfElse ex) (Expression ex) (Expression ex) (Expression ex) Loc
  -- ^ if with else clause
  --
  -- if expr_boolean:
  --   ture_expr
  -- else:
  --   false_expr
  --
  | For (XFor ex) (XName ex) (Expression ex) (Expression ex) Loc
  -- ^ for
  | Apply (XApply ex) (XAccessor ex) [Expression ex] Loc
  -- ^ Make a call to a function with parameters given
  --
  -- func_apply arg1 (expr) arg3
  --
  | Identifier (XIdentifier ex) (XAccessor ex) Loc
  -- ^ Allows retrieve a value from memory given an address "Accessor"
  | Factor (XFactor ex) (Atom ex) Loc
  -- ^ Raw Factors

data Accessor a
  = Dot T.Text (Accessor a) Loc
  | Simple T.Text Loc

data Atom a
  = ANum Int Loc
  | AClass T.Text [(T.Text, Expression a)] Loc
  | ADecimal Double Loc
  | ARegex T.Text Loc
  | AShellCommand T.Text Loc
  | AStr T.Text Loc
  | ABool Bool Loc
  | AVector [Expression a] Loc
  | ADic [(T.Text, Expression a)] Loc
  | ANone Loc

-- * Type families
type family XName ex
type family XAccessor ex

type family XFunExpr ex
type family XVarExpr ex
type family XSeqExpr ex
type family XMkScope ex
type family XIf ex
type family XIfElse ex
type family XFor ex
type family XApply ex
type family XIdentifier ex
type family XFactor ex

data Empty

data Tok
type ExprTok = Expression Tok

type instance XName Tok = T.Text
type instance XAccessor Tok = Accessor Tok

type instance XFunExpr Tok = Empty
type instance XVarExpr Tok = Empty
type instance XSeqExpr Tok = Empty
type instance XMkScope Tok = Empty
type instance XIf Tok = Empty
type instance XIfElse Tok = Empty
type instance XFor Tok = Empty
type instance XApply Tok = Empty
type instance XIdentifier Tok = Empty
type instance XFactor Tok = Empty

type ForallExt (c :: * -> Constraint) ex =
  ( c (XName ex)
  , c (XAccessor ex)
  , c (XFunExpr ex)
  , c (XVarExpr ex)
  , c (XSeqExpr ex)
  , c (XMkScope ex)
  , c (XIf ex)
  , c (XIfElse ex)
  , c (XFor ex)
  , c (XApply ex)
  , c (XIdentifier ex)
  , c (XFactor ex)
  )

absurb :: Empty
absurb = error "Error to evaluate a Void into AST"

pattern TokFunExpr :: [T.Text] -> ExprTok -> Loc -> ExprTok
pattern TokFunExpr acc expr loc <- FunExpr _ acc expr loc
  where TokFunExpr acc expr loc = FunExpr absurb acc expr loc

pattern TokVarExpr :: Accessor Tok -> ExprTok -> Loc -> ExprTok
pattern TokVarExpr acc expr loc <- VarExpr _ acc expr loc
  where TokVarExpr acc expr loc = VarExpr absurb acc expr loc

pattern TokSeqExpr :: [ExprTok] -> Loc -> ExprTok
pattern TokSeqExpr exprs loc <- SeqExpr _ exprs loc
  where TokSeqExpr exprs loc = SeqExpr absurb exprs loc

pattern TokMkScope :: [ExprTok] -> Loc -> ExprTok
pattern TokMkScope exprs loc <- MkScope _ exprs loc
  where TokMkScope exprs loc = MkScope absurb exprs loc

pattern TokIf :: ExprTok -> ExprTok -> Loc -> ExprTok
pattern TokIf expr trueExpr loc <- If _ expr trueExpr loc
  where TokIf expr trueExpr loc = If absurb expr trueExpr loc

pattern TokIfElse :: ExprTok -> ExprTok -> ExprTok -> Loc -> ExprTok
pattern TokIfElse expr trueExpr falseExpr loc <- IfElse _ expr trueExpr falseExpr loc
  where TokIfElse expr trueExpr falseExpr loc = IfElse absurb expr trueExpr falseExpr loc

pattern TokFor :: T.Text -> ExprTok -> ExprTok -> Loc -> ExprTok
pattern TokFor name iterExpr bodyExpr loc <- For _ name iterExpr bodyExpr loc
  where TokFor name iterExpr bodyExpr loc = For absurb name iterExpr bodyExpr loc

pattern TokApply :: Accessor Tok -> [ExprTok] -> Loc -> ExprTok
pattern TokApply acc exprs loc <- Apply _ acc exprs loc
  where TokApply acc exprs loc = Apply absurb acc exprs loc

pattern TokIdentifier :: Accessor Tok -> Loc -> ExprTok
pattern TokIdentifier acc loc <- Identifier _ acc loc
  where TokIdentifier acc loc = Identifier absurb acc loc

pattern TokFactor :: Atom Tok -> Loc -> ExprTok
pattern TokFactor atom loc <- Factor _ atom loc
  where TokFactor atom loc = Factor absurb atom loc


lvlIndent :: Int
lvlIndent = 2

instance Pretty Empty where
  pretty _ = mempty

instance Pretty Repl where
  pretty repl = case repl of
    Command name args -> pretty name <+> hsep (map pretty args)
    Code exprs        -> vcat $ map pretty exprs

instance ForallExt Pretty ex => Pretty (Statement ex) where
  pretty statement = case statement of
    ClassSt classDecl ->
      vsep [ "ClassSt {"
           , indent lvlIndent (pretty classDecl)
           , "}"
           ]
    FunSt funDecl     ->
      vsep [ "FunSt {"
           , indent lvlIndent (pretty funDecl)
           , "}"
           ]
    Expr expr         ->
      vsep [ "Expr {"
           , indent lvlIndent (pretty expr)
           , "}"
           ]

instance ForallExt Pretty ex => Pretty (ClassDecl ex) where
  pretty (ClassDecl name methods loc) =
    vsep [ "ClassDecl" <+> pretty name
           <+> ("(" <> pretty loc <> ")")
           <+> "{"
         , indent lvlIndent (vcat (map pretty methods))
         , "}"
         ]

instance ForallExt Pretty ex => Pretty (FunDecl ex) where
  pretty (FunDecl name args body loc) =
    vsep [ "FunDecl" <+> pretty name
           <+> "Args:"
           <+> pretty args
           <+> ("(" <> pretty loc <> ")")
           <+> "{"
         , indent lvlIndent (pretty body)
         , "}"
         ]

instance ForallExt Pretty ex => Pretty (Expression ex) where
  pretty expr = case expr of
    FunExpr xInfo args body loc ->
      vsep [ "FunExpr" <+> "args:"
             <+> pretty args
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (pretty body)
           , "}"
           ]
    VarExpr xInfo acc expr' loc ->
      vsep [ "VarExpr" <+> pretty acc
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (pretty expr')
           , "}"
           ]
    MkScope xInfo exprs loc ->
      vsep [ "MkScope"
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (vcat (map pretty exprs))
           , "}"
           ]
    SeqExpr xInfo exprs loc ->
      vsep [ "SeqExpr"
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (vcat (map pretty exprs))
           , "}"
           ]
    If xInfo cond trueExpr loc ->
      vsep [ "If" <+> pretty cond
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (pretty trueExpr)
           , "}"
           ]
    IfElse xInfo cond trueExpr falseExpr loc ->
      vsep [ "IfElse" <+> pretty cond
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (pretty trueExpr)
           , "} Else {"
           , indent lvlIndent (pretty falseExpr)
           , "}"
           ]
    For xInfo name loopExpr body loc ->
      vsep [ "For" <+> pretty name <> pretty loopExpr
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (pretty body)
           , "}"
           ]
    Apply xInfo acc exprs loc ->
      vsep [ "Apply" <+> pretty acc
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (vcat (map (\(num, expr') ->
               (pretty (num :: Int))
                <+> "->"
                <+> pretty expr') (zip [1..] exprs)))
           , "}"
           ]
    Identifier xInfo acc loc ->
      vsep [ "Identifier"
             <+> ("(" <> pretty loc <> ")")
             <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (pretty acc)
           , "}"
           ]
    Factor xInfo atom loc ->
      vsep [ "Factor"
              <+> ("(" <> pretty loc <> ")")
              <+> "{" <+> align (pretty xInfo)
           , indent lvlIndent (pretty atom)
           , "}"
           ]

instance ForallExt Pretty ex => Pretty (Accessor ex) where
  pretty acc = case acc of
    Dot name accessor _ ->
      pretty name <>  "." <> pretty accessor
    Simple name _ ->  pretty name

instance ForallExt Pretty ex => Pretty (Atom ex) where
  pretty atom = case atom of
    ANum num _           -> "Num" <+> pretty num
    ADecimal num _       -> "Decimal" <+> pretty num
    ARegex reg _         -> "Regex" <+> pretty reg
    AShellCommand comm _ -> "Command" <+> pretty comm
    AStr str _           -> "Str" <+> pretty str
    ABool bool _        -> "Bool" <+> pretty bool
    AVector vec _       -> "Vector" <+> hsep (map pretty vec)
    AClass nameClass dic _ ->
      "Class" <+>  pretty nameClass <+> "{" <>
      indent lvlIndent (vcat (map (\(name, expr) ->
        pretty name <>  ":" <+> pretty expr) dic))
      <>  "}"
    ADic dic _          ->
      "Dic {" <>
      indent lvlIndent (vcat (map (\(name, expr) ->
        pretty name <>  ":" <+> pretty expr) dic))
      <>  "}"
    ANone _             ->  "None"

makeSuffixLenses ''FunDecl
makeSuffixLenses ''ClassDecl
