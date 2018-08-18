{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Ast where

import           Compiler.Parser.Types
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

import           Compiler.Prettify
import           Compiler.Utils


data Repl
  = Command T.Text [T.Text]
  | Code [Statement TokenInfo]
  deriving Show

data Statement a
  = Import T.Text a
  | ClassSt (ClassDecl a)
  | FunSt (FunDecl a)
  | Expr (Expression a)
  deriving Show

data ClassDecl a = ClassDecl
  { _clsName    :: T.Text
  , _clsMethods :: [FunDecl a]
  , _clsToken   :: a
  } deriving Show

data FunDecl a = FunDecl
  { _funName  :: T.Text
  , _funArgs  :: [T.Text]
  , _funBody  :: Expression a
  , _funToken :: a
  } deriving Show

-- | Generic representation of expression
data Expression a
  = FunExpr [T.Text] (Expression a) a
  -- Create a lambda function
  | VarExpr (Accessor a) (Expression a) a
  -- ^ Creates a new variable
  | SeqExpr [Expression a] a
  -- ^ Group up expression without use a new scope
  | MkScope [Expression a] a
  -- ^ Creates a scope
  | If (Expression a) (Expression a) a
  -- ^ Simple if
  | IfElse (Expression a) (Expression a) (Expression a) a
  -- ^ if else
  | For T.Text (Expression a) (Expression a) a
  -- ^ for
  | Apply (Accessor a) [Expression a] a
  -- ^ Make a call to a function with parameters given
  | Identifier (Accessor a) a
  -- ^ Allows retrieve a value from memory given an address "Accessor"
  | Factor (Atom a) a
  -- ^ Raw Factors
  deriving Show

data Accessor a
  = Dot T.Text (Accessor a) a
  | Simple T.Text a
  deriving Show

data Atom a
  = ANum Int a
  | AClass T.Text [(T.Text, Expression a)] a
  | ADecimal Double a
  | ARegex T.Text a
  | AShellCommand T.Text a
  | AStr T.Text a
  | ABool Bool a
  | AVector [Expression a] a
  | ADic [(T.Text, Expression a)] a
  | ANone a
  deriving Show

instance Prettify Repl where
  prettify repl verbose = case repl of
    Command name args -> pretty name <+> hsep (map pretty args)
    Code exprs        -> vcat $ map (`prettify` verbose) exprs

instance Prettify a => Prettify (Statement a) where
  prettify statement verbose = case statement of
    Import path _     -> "Import {" <+> pretty path <+> "}"
    ClassSt classDecl -> "ClassSt {" <> line <> nest 2 (prettify classDecl verbose) <> line <> "}"
    FunSt funDecl     -> "FunSt {" <> line <> nest 2 (prettify funDecl verbose) <> line <> "}"
    Expr expr         -> "Expr {" <> line <> nest 2 (prettify expr verbose) <> line <> "}"

instance Prettify a => Prettify (ClassDecl a) where
  prettify (ClassDecl name methods a) verbose =
    "ClassDecl" <+> pretty name <+>  "{" <+>
    prettify a verbose <> line <>
    nest 2 (vcat (map (`prettify` verbose) methods)) <> line <>
    "}"

instance Prettify a => Prettify (FunDecl a) where
  prettify (FunDecl name args body a) verbose =
    "FunDecl" <+> pretty name <+>
    "Args:" <+> hsep (map pretty args) <+> "{" <+>
    prettify a verbose <> line <>
    nest 2 (prettify body verbose) <> line <>
    "}"

instance Prettify a => Prettify (Expression a) where
  prettify expr verbose = case expr of
    FunExpr args body a ->
      "FunExpr" <+>  "args:" <+> hsep (map pretty args) <+> "{" <+>
      prettify a verbose <> line <>
      nest 2 (prettify body verbose) <> line <>
      "}"
    VarExpr acc expr' a ->
      "VarExpr" <+> prettify acc verbose <+> "{" <+>
      prettify a verbose <> line <>
      nest 2 (prettify expr' verbose) <> line <>
      "}"
    MkScope exprs a ->
      "MkScope {" <+> prettify a verbose <> line <>
      nest 2 (vcat (map (`prettify` verbose) exprs)) <> line <>
      "}"
    SeqExpr exprs a ->
      "SeqExpr {" <+> prettify a verbose <> line <>
      nest 2 (vcat (map (`prettify` verbose) exprs)) <> line <>
      "}"
    If cond trueExpr a ->
      "If" <+> prettify cond verbose <+>  "{" <+> prettify a verbose <> line <>
      nest 2 (prettify trueExpr verbose) <> line <>
      "}"
    IfElse cond trueExpr falseExpr a ->
      "IfElse" <+> prettify cond verbose <+>  "{" <+>
      prettify a verbose <> line <>
      nest 2 (prettify trueExpr verbose) <> line <>
      "} Else {" <> line <>
      nest 2 (prettify falseExpr verbose) <> line <>
      "}"
    For name loopExpr body a ->
      "For" <+> pretty name <> prettify loopExpr verbose <+>
      "{" <+> prettify a verbose <> line <>
      nest 2 (prettify body verbose) <> line <>
      "}"
    Apply acc exprs a ->
      "Apply" <+> prettify acc verbose <+> prettify a verbose <> line <>
      nest 2 (vcat (map (\(num, expr') ->
         (pretty (num :: Int))
          <+> "->"
          <+> prettify expr' verbose) (zip [1..] exprs)))
    Identifier acc a ->
       "Identifier" <+> prettify acc verbose <+> prettify a verbose
    Factor atom a ->
       "Factor" <+> prettify atom verbose <+> prettify a verbose

instance Prettify a => Prettify (Accessor a) where
  prettify acc verbose = case acc of
    Dot name accessor _ ->
      pretty name <>  "." <> prettify accessor verbose
    Simple name _ ->  pretty name

instance Prettify a => Prettify (Atom a) where
  prettify atom verbose = case atom of
    ANum num _           -> "Num" <+> pretty num
    ADecimal num _       -> "Decimal" <+> pretty num
    ARegex reg _         -> "Regex" <+> pretty reg
    AShellCommand comm _ -> "Command" <+> pretty comm
    AStr str _           -> "Str" <+> pretty str
    ABool bool _        -> "Bool" <+> pretty bool
    AVector vec _       -> "Vector" <+> hsep (map (`prettify` verbose) vec)
    AClass nameClass dic _ ->
      "Class" <+>  pretty nameClass <+> "{" <>
      nest 2 (vcat (map (\(name, expr) ->
        pretty name <>  ":" <+> prettify expr verbose) dic))
      <>  "}"
    ADic dic _          ->
      "Dic {" <>
      nest 2 (vcat (map (\(name, expr) ->
        pretty name <>  ":" <+> prettify expr verbose) dic))
      <>  "}"
    ANone _             ->  "None"

makeSuffixLenses ''FunDecl
makeSuffixLenses ''ClassDecl
