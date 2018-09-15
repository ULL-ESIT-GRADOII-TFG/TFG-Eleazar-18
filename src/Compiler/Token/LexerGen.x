{
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_HADDOCK prune #-}
module Compiler.Token.LexerGen where

import qualified Data.Vector as V
import qualified Data.Text as T

import Compiler.Token.Types
import Compiler.Token.Methods

}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [_a-zA-Z]
$alphaDigit = [_a-zA-Z0-9]
$operators = [\\\/\>\<\!\@\=\$\%\&\?\+\-\*\.\^\|]

@nameId = $alpha $alphaDigit*
@classId = [A-Z] @nameId?

@emptyLines = ([\ \t]*("#".*)?\n)+
@comments = ([\ \t]*"#".*\n)+
@number = $digit+
@decimal = @number "." $digit+


tokens :-

  <0> {
    :.+           { mkL' (\text -> let (command:args) = T.words $ T.drop 1 text in ICommandT command args) }
    $white*       { begin code_st }
  }

  <empty_lines> {
    @emptyLines    { begin inc_indent}
  }

  <inc_indent> {
    [\ \t]*       { newIndent `andBegin` code_st }
  }

  <code_st> {
    "#".*         ;  -- Comment Line
    [\ \t]+       ;
    ":"           { mkL OBraceT `andBegin` empty_lines }
    \n            { begin dec_indent }
    "fun"         { mkL FunT }
    "lam"         { mkL LamT }
    "in"          { mkL InT }
    "for"         { mkL ForT }
    "if"          { mkL IfT }
    "else"        { mkL ElseT }
    "class"       { mkL ClassT }
    "use"         { mkL UseT }
    "cd"          { mkL CdT }
    "["           { mkGroupOL OBracketT }
    "]"           { mkGroupCL OBracketT CBracketT }
    "{"           { mkGroupOL OBraceT }
    "}"           { mkGroupCL OBraceT CBraceT }
    "("           { mkGroupOL OParenT }
    ")"           { mkGroupCL OParenT CParenT }
    ","           { mkL CommaT }
    ";"           { mkL EndStmtT }
    "="           { mkL AssignT }
    "none"        { mkL NoneT }
    "true"        { mkL (BoolT True) }
    "false"       { mkL (BoolT False) }
    \"            { kindString' Str string }
    !\$           { mkL (OperatorT $ T.pack "!")
                    `andAction` kindString Shell
                    `andBegin` shell }
    \$            { kindString' Shell shell }
    !\$\"         { mkL (OperatorT $ T.pack "!")
                    `andAction` kindString ShellStr
                    `andBegin` shell_alternative }
    \$\"          { kindString' ShellStr shell_alternative }
    "r/"          { kindString' Reg regex }
    $operators+   { mkL' OperatorT }
    @number       { mkL' (NumT . read . T.unpack) }
    @decimal      { mkL' (DecimalT . read . T.unpack) }
    @classId      { mkL' ClassIdT }
    @nameId       { mkL' NameIdT }
  }

  <dec_indent> {
    @emptyLines   ;
    [\ \t]*       { checkDecrement `andBegin` code_st }
  }

  <string> {
    \\\"          { skipJustAdd "\"" }
    \\n           { skipJustAdd "\n" }
    \\t           { skipJustAdd "\t" }
    \n            { skipJustAdd "\n" }
    \t            { skipJustAdd "\t" }
    [^\"]         { addToInnerString }
    \"            { generateLexerFromInner LitTextT `andBegin` code_st }
  }

  <shell> {
    "#".*         ;  -- Comment Line
    \\n           { skipJustAdd "\n" }
    \\t           { skipJustAdd "\t" }
    \t            { skipJustAdd "\t" }
    [^\n]         { addToInnerString }
    \n            { generateLexerFromInner ShellCommandT `andBegin` code_st }
  }

  <shell_alternative> {
    \\\"          { skipJustAdd "\"" }
    \\n           { skipJustAdd "\n" }
    \\t           { skipJustAdd "\t" }
    \n            { skipJustAdd "\n" }
    \t            { skipJustAdd "\t" }
    [^\"]         { addToInnerString }
    \"            { generateLexerFromInner ShellCommandT `andBegin` code_st }
  }

  <regex> {
    \\\"           { skipJustAdd "\"" }
    "\/"           { skipJustAdd "/" }
    \\n            { skipJustAdd "\n" }
    \\t            { skipJustAdd "\t" }
    [^\/]          { addToInnerString }
    "/"            { generateLexerFromInner RegexExprT `andBegin` code_st }
  }
