{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Compiler.Token.Methods where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)
import           Text.Parsec
import           Text.Parsec.Pos

import Compiler.Token.Types
import qualified Compiler.Token.Lexer as L


instance (Monad m) => Stream (Vector tok) m tok where
    uncons stream =
      if V.null stream then
        return Nothing
      else
        return $ Just (V.unsafeHead stream, V.tail stream)
    {-# INLINE uncons #-}

type TokenParser a = Parsec (Vector L.Lexeme) () a

match :: Token -> TokenParser ()
match tok = do
  lastPos <- getPosition
  token
    show
    (\(L.L (L.AlexPn _ l c) _) -> newPos (sourceName lastPos) l c)
    (\(L.L _ tok') -> if tok == tok' then Just () else Nothing)

match' :: (Token -> Maybe a) -> TokenParser a
match' fun = do
  lastPos <- getPosition
  token
    show
    (\(L.L (L.AlexPn _ l c) _) -> newPos (sourceName lastPos) l c)
    (\(L.L _ tok') -> fun tok')

cBraceT :: TokenParser ()
cBraceT = match CBraceT

oBraceT :: TokenParser ()
oBraceT = match OBraceT

cParenT :: TokenParser ()
cParenT = match CParenT

oParenT :: TokenParser ()
oParenT = match OParenT

classT :: TokenParser ()
classT = match ClassT

importT :: TokenParser ()
importT = match ImportT

lamT :: TokenParser ()
lamT = match LamT

funT :: TokenParser ()
funT = match FunT

forT :: TokenParser ()
forT = match ForT

inT :: TokenParser ()
inT = match InT

ifT :: TokenParser ()
ifT = match IfT

elseT :: TokenParser ()
elseT = match ElseT

commaT :: TokenParser ()
commaT = match CommaT

assignT :: TokenParser ()
assignT = match AssignT

shellCommandT :: TokenParser Text
shellCommandT = match' $
  \case
    ShellCommandT command -> Just command
    _                     -> Nothing

regexExprT :: TokenParser Text
regexExprT = match' $
  \case
    RegexExprT regex -> Just regex
    _                -> Nothing

nameIdT :: TokenParser Text
nameIdT = match' $
  \case
    NameIdT name -> Just name
    _            -> Nothing

litTextT :: TokenParser Text
litTextT = match' $
  \case
    LitTextT name -> Just name
    _             -> Nothing

numberT :: TokenParser Int
numberT = match' $
  \case
    NumT num -> Just num
    _        -> Nothing

operatorT :: TokenParser Text
operatorT = match' $
  \case
    OperatorT op -> Just op
    _            -> Nothing

exitT :: TokenParser ()
exitT = match (ICommandT "exit" [])

helpT :: TokenParser ()
helpT = match (ICommandT "help" [])

