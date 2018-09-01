{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
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
  -- ^ Create a lambda function
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

lvlIndent :: Int
lvlIndent = 2

instance Prettify Repl where
  prettify repl verbose = case repl of
    Command name args -> pretty name <+> hsep (map pretty args)
    Code exprs        -> vcat $ map (`prettify` verbose) exprs

instance Prettify a => Prettify (Statement a) where
  prettify statement verbose = case statement of
    Import path _     -> "Import {" <+> pretty path <+> "}"
    ClassSt classDecl ->
      vsep [ "ClassSt {"
           , indent lvlIndent (prettify classDecl verbose)
           , "}"
           ]
    FunSt funDecl     ->
      vsep [ "FunSt {"
           , indent lvlIndent (prettify funDecl verbose)
           , "}"
           ]
    Expr expr         ->
      vsep [ "Expr {"
           , indent lvlIndent (prettify expr verbose)
           , "}"
           ]

instance Prettify a => Prettify (ClassDecl a) where
  prettify (ClassDecl name methods a) verbose =
    vsep [ "ClassDecl" <+> pretty name <+>  "{" <+> align (prettify a verbose)
         , indent lvlIndent (vcat (map (`prettify` verbose) methods))
         , "}"
         ]

instance Prettify a => Prettify (FunDecl a) where
  prettify (FunDecl name args body a) verbose =
    vsep [ "FunDecl" <+> pretty name <+> "Args:" <+> hsep (map pretty args) <+> "{" <+> align (prettify a verbose)
         , indent lvlIndent (prettify body verbose)
         , "}"
         ]

instance Prettify a => Prettify (Expression a) where
  prettify expr verbose = case expr of
    FunExpr args body a ->
      vsep [ "FunExpr" <+>  "args:" <+> hsep (map pretty args) <+> "{" <+> align (prettify a verbose)
           , indent lvlIndent (prettify body verbose)
           , "}"
           ]
    VarExpr acc expr' a ->
      vsep [ "VarExpr" <+> prettify acc verbose <+> "{" <+> prettify a verbose
           , indent lvlIndent (prettify expr' verbose)
           , "}"
           ]
    MkScope exprs a ->
      vsep [ "MkScope {" <+> align (prettify a verbose)
           , indent lvlIndent (vcat (map (`prettify` verbose) exprs))
           , "}"
           ]
    SeqExpr exprs a ->
      vsep [ "SeqExpr {" <+> align (prettify a verbose)
           , indent lvlIndent (vcat (map (`prettify` verbose) exprs))
           , "}"
           ]
    If cond trueExpr a ->
      vsep [ "If" <+> prettify cond verbose <+> "{" <+> align (prettify a verbose)
           , indent lvlIndent (prettify trueExpr verbose)
           , "}"
           ]
    IfElse cond trueExpr falseExpr a ->
      vsep [ "IfElse" <+> prettify cond verbose <+>  "{" <+> align (prettify a verbose)
           , indent lvlIndent (prettify trueExpr verbose)
           , "} Else {"
           , indent lvlIndent (prettify falseExpr verbose)
           , "}"
           ]
    For name loopExpr body a ->
      vsep [ "For" <+> pretty name <> prettify loopExpr verbose <+> "{" <+> align (prettify a verbose)
           , indent lvlIndent (prettify body verbose)
           , "}"
           ]
    Apply acc exprs a ->
      vsep [ "Apply" <+> prettify acc verbose <+> align (prettify a verbose)
           , indent lvlIndent (vcat (map (\(num, expr') ->
               (pretty (num :: Int))
                <+> "->"
                <+> prettify expr' verbose) (zip [1..] exprs)))
           ]
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
      indent lvlIndent (vcat (map (\(name, expr) ->
        pretty name <>  ":" <+> prettify expr verbose) dic))
      <>  "}"
    ADic dic _          ->
      "Dic {" <>
      indent lvlIndent (vcat (map (\(name, expr) ->
        pretty name <>  ":" <+> prettify expr verbose) dic))
      <>  "}"
    ANone _             ->  "None"

makeSuffixLenses ''FunDecl
makeSuffixLenses ''ClassDecl
