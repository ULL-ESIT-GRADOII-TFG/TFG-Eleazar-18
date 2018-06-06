{-# LANGUAGE TemplateHaskell #-}
module Compiler.Ast where

import           Compiler.Parser.Types
import qualified Data.Text             as T
import           Lens.Micro.Platform
import           Text.PrettyPrint


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
  | VarExpr (Accessor a) (Expression a) a
  | SeqExpr [Expression a] a
  | If (Expression a) (Expression a) a
  | IfElse (Expression a) (Expression a) (Expression a) a
  | For T.Text (Expression a) (Expression a) a
  | MkScope [Expression a]
  -- ^ Explicit scope
  | Apply (Accessor a) [Expression a] a
  | Identifier (Accessor a) a
  | Factor (Atom a) a
  deriving Show

data Accessor a
  = Dot T.Text (Accessor a) a
  | Simple T.Text a
  deriving Show

data Atom a
  = ANum Int
  | ADecimal Double
  | ARegex T.Text
  | AShellCommand T.Text
  | AStr T.Text
  | ABool Bool
  | AVector [Expression a]
  | ADic [(T.Text, Expression a)]
  | ANone
  deriving Show


class PrettyAst a where
  prettify :: a -> Int -> Doc

instance PrettyAst Repl where
  prettify repl verbose = case repl of
    Command name args -> text (T.unpack name) <> text " " <> hsep (map (text . T.unpack) args)
    Code exprs -> vcat $ map (`prettify` verbose) exprs

instance PrettyAst (Statement a) where
  prettify statement verbose = case statement of
    Import path _ -> text "Import { " <> text (T.unpack path) <> text " }"
    ClassSt classDecl -> text "ClassSt {" $$ nest 2 (prettify classDecl verbose) $$ text "}"
    FunSt funDecl -> text "FunSt {" $$ nest 2 (prettify funDecl verbose) $$ text "}"
    Expr expr -> text "FunSt {" $$ nest 2 (prettify expr verbose) $$ text "}"

instance PrettyAst (ClassDecl a) where
  prettify (ClassDecl name methods _) verbose =
    text "ClassDecl " <> text (T.unpack name) <> text " {"
      $$ nest 2 (vcat (map (`prettify` verbose) methods)) $$ text "}"

instance PrettyAst (FunDecl a) where
  prettify (FunDecl name args body _) verbose =
    text "FunDecl " <> text (T.unpack name)
      <> text " args: " <> hsep (map (text . T.unpack) args) <> text " {"
      $$ nest 2 (prettify body verbose) $$ text "}"

instance PrettyAst (Expression a) where
  prettify expr verbose = case expr of
    FunExpr args expr _ ->
      text "FunExpr " <> text " args: " <> hsep (map (text . T.unpack) args) <> text " {"
        $$ nest 2 (prettify expr verbose) $$ text "}"
    VarExpr acc expr _ ->
      text "VarExpr " <> prettify acc verbose <> text " {"
        $$ nest 2 (prettify expr verbose) $$ text "}"
    SeqExpr exprs _ ->
      text "SeqExpr {" $$ nest 2 (vcat (map (`prettify` verbose) exprs)) $$ text "}"
    If cond trueExpr _ ->
      text "If " <> prettify cond verbose <> text " {"
        $$ nest 2 (prettify trueExpr verbose) $$ text "}"
    IfElse cond trueExpr falseExpr _ ->
      text "IfElse " <> prettify cond verbose <> text " {"
        $$ nest 2 (prettify trueExpr verbose) $$ text "} Else {"
        $$ nest 2 (prettify falseExpr verbose) $$ text "}"
    For name loopExpr body _ ->
      text "For " <> text (T.unpack name) <> prettify loopExpr verbose <>text " {"
        $$ nest 2 (prettify body verbose) $$ text "}"
    MkScope exprs ->
      text "MkScope {" $$ nest 2 (vcat (map (`prettify` verbose) exprs)) $$ text "}"
    Apply acc exprs _ ->
      text "Apply " <> prettify acc verbose $$ nest 2 (vcat (map (\(num, expr) -> text (show num) <> prettify expr verbose) (zip [1..] exprs)))
    Identifier acc _ ->
      text "Identifier " <> prettify acc verbose
    Factor atom _ ->
      text "Factor " <> prettify atom verbose

instance PrettyAst (Accessor a) where
  prettify acc verbose = case acc of
    Dot name acc _ -> text (T.unpack name) <> text "." <> prettify acc verbose
    Simple name _ -> text (T.unpack name)

instance PrettyAst (Atom a) where
  prettify atom verbose = case atom of
    ANum num           -> text "Num " <> text (show num)
    ADecimal num       -> text "Decimal " <> text (show num)
    ARegex reg         -> text "Regex " <> text (T.unpack reg)
    AShellCommand comm -> text "Command " <> text (T.unpack comm)
    AStr str           -> text "Str " <> text (T.unpack str)
    ABool bool         -> text "Bool " <> text (show bool)
    AVector vec        -> text "Vector " <> hsep (map (`prettify` verbose) vec)
    ADic dic           -> text "Dic {" <> nest 2 (vcat (map (\(name, expr) -> text (T.unpack name) <> text ": " <> prettify expr verbose) dic)) <> text "}"
    ANone              -> text "None"

makeLenses ''FunDecl
makeLenses ''ClassDecl
