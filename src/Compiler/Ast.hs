{-# LANGUAGE TemplateHaskell #-}
module Compiler.Ast where

import           Compiler.Parser.Types
import qualified Data.Text             as T
import           Text.PrettyPrint

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
    Command name args -> text (T.unpack name) <> text " " <> hsep (map (text . T.unpack) args)
    Code exprs -> vcat $ map (`prettify` verbose) exprs

instance Prettify a => Prettify (Statement a) where
  prettify statement verbose = case statement of
    Import path _ -> text "Import { " <> text (T.unpack path) <> text " }"
    ClassSt classDecl -> text "ClassSt {" $$ nest 2 (prettify classDecl verbose) $$ text "}"
    FunSt funDecl -> text "FunSt {" $$ nest 2 (prettify funDecl verbose) $$ text "}"
    Expr expr -> text "Expr {" $$ nest 2 (prettify expr verbose) $$ text "}"

instance Prettify a => Prettify (ClassDecl a) where
  prettify (ClassDecl name methods a) verbose =
    text "ClassDecl " <> text (T.unpack name) <> text " { " <>
    prettify a verbose $$
    nest 2 (
             vcat (map (`prettify` verbose) methods)) $$
    text "}"

instance Prettify a => Prettify (FunDecl a) where
  prettify (FunDecl name args body a) verbose =
    text "FunDecl " <> text (T.unpack name) <>
    text " Args: " <> hsep (map (text . T.unpack) args) <> text " { " <>
    prettify a verbose $$
    nest 2 (prettify body verbose) $$ text "}"

instance Prettify a => Prettify (Expression a) where
  prettify expr verbose = case expr of
    FunExpr args body a ->
      text "FunExpr " <> text " args: " <> hsep (map (text . T.unpack) args) <>
      text " { " <> prettify a verbose $$
      nest 2 (prettify body verbose) $$ text "}"
    VarExpr acc expr' a ->
      text "VarExpr " <> prettify acc verbose <> text " { " <>
      prettify a verbose $$
      nest 2 (prettify expr' verbose) $$
      text "}"
    MkScope exprs a ->
      text "MkScope { " <> prettify a verbose $$
      nest 2 (vcat (map (`prettify` verbose) exprs)) $$
      text "}"
    SeqExpr exprs a ->
      text "SeqExpr { " <> prettify a verbose $$
      nest 2 (vcat (map (`prettify` verbose) exprs)) $$
      text "}"
    If cond trueExpr a ->
      text "If " <> prettify cond verbose <> text " { " <> prettify a verbose $$
      nest 2 (prettify trueExpr verbose) $$ text "}"
    IfElse cond trueExpr falseExpr a ->
      text "IfElse " <> prettify cond verbose <> text " { " <>
      prettify a verbose $$
      nest 2 (prettify trueExpr verbose) $$ text "} Else {" $$
      nest 2 (prettify falseExpr verbose) $$ text "}"
    For name loopExpr body a ->
      text "For " <> text (T.unpack name) <> prettify loopExpr verbose <>
      text " { " <> prettify a verbose $$
      nest 2 (prettify body verbose) $$ text "}"
    Apply acc exprs a ->
      text "Apply " <> prettify acc verbose <> text " " <> prettify a verbose $$
      nest 2 (vcat (map (\(num, expr') ->
        text (show (num :: Int))
          <> text " -> "
          <> prettify expr' verbose) (zip [1..] exprs)))
    Identifier acc a ->
      text "Identifier " <> prettify acc verbose <> text " " <> prettify a verbose
    Factor atom a ->
      text "Factor " <> prettify atom verbose <> text " " <> prettify a verbose

instance Prettify a => Prettify (Accessor a) where
  prettify acc verbose = case acc of
    Dot name accessor _ ->
      text (T.unpack name) <> text "." <> prettify accessor verbose
    Simple name _ -> text (T.unpack name)

instance Prettify a => Prettify (Atom a) where
  prettify atom verbose = case atom of
    ANum num _           -> text "Num " <> text (show num)
    ADecimal num _       -> text "Decimal " <> text (show num)
    ARegex reg _         -> text "Regex " <> text (T.unpack reg)
    AShellCommand comm _ -> text "Command " <> text (T.unpack comm)
    AStr str _           -> text "Str " <> text (T.unpack str)
    ABool bool _        -> text "Bool " <> text (show bool)
    AVector vec _       -> text "Vector " <> hsep (map (`prettify` verbose) vec)
    AClass nameClass dic _ ->
      text "Class " <> text (T.unpack nameClass) <> text " {" <>
      nest 2 (vcat (map (\(name, expr) ->
        text (T.unpack name) <> text ": " <> prettify expr verbose) dic))
      <> text "}"
    ADic dic _          -> text "Dic {" <>
      nest 2 (vcat (map (\(name, expr) ->
        text (T.unpack name) <> text ": " <> prettify expr verbose) dic))
      <> text "}"
    ANone _             -> text "None"

makeSuffixLenses ''FunDecl
makeSuffixLenses ''ClassDecl
