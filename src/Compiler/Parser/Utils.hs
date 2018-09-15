{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Compiler.Parser.Utils where

import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Text.Parsec
import           Text.Parsec.Pos

import           Compiler.Token.Types

instance (Monad m) => Stream (Vector tok) m tok where
    uncons stream =
      if V.null stream then
        return Nothing
      else
        return $ Just (V.unsafeHead stream, V.tail stream)
    {-# INLINE uncons #-}

type TokenParser a = Parsec (Vector Lexeme) () a

-- TODO Doesnt allow marks start and end of token
match :: Token -> TokenParser ()
match tok = do
  lastPos <- getPosition
  token
    (\(L _ _ tok') -> show tok')
    (\(L (AlexPn _ l c) _ _) -> newPos (sourceName lastPos) l c)
    (\(L _ _ tok') -> if tok == tok' then Just () else Nothing)

match' :: (Token -> Maybe a) -> TokenParser a
match' fun = do
  lastPos <- getPosition
  token
    (\(L _ _ tok') -> show tok')
    (\(L (AlexPn _ l c) _ _) -> newPos (sourceName lastPos) l c)
    (\(L _ _ tok') -> fun tok')

cBraceT :: TokenParser ()
cBraceT = match CBraceT

oBraceT :: TokenParser ()
oBraceT = match OBraceT

cBracketT :: TokenParser ()
cBracketT = match CBracketT

oBracketT :: TokenParser ()
oBracketT = match OBracketT

cParenT :: TokenParser ()
cParenT = match CParenT

oParenT :: TokenParser ()
oParenT = match OParenT

classT :: TokenParser ()
classT = match ClassT

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

endStmtT :: TokenParser ()
endStmtT = match EndStmtT

noneT :: TokenParser ()
noneT = match NoneT

boolT :: TokenParser Bool
boolT = match' $
  \case
    BoolT bool -> Just bool
    _          -> Nothing

shellCommandT :: TokenParser Text
shellCommandT = match' $
  \case
    ShellCommandT command -> Just command
    _                     -> Nothing

regexT :: TokenParser Text
regexT = match' $
  \case
    RegexExprT regex -> Just regex
    _                -> Nothing

nameIdT :: TokenParser Text
nameIdT = match' $
  \case
    NameIdT name -> Just name
    _            -> Nothing

classIdT :: TokenParser Text
classIdT = match' $
  \case
    ClassIdT name -> Just name
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

decimalT :: TokenParser Double
decimalT = match' $
  \case
    DecimalT num -> Just num
    _        -> Nothing

operatorT :: TokenParser Text
operatorT = match' $
  \case
    OperatorT op -> Just op
    _            -> Nothing

operatorT' :: Text -> TokenParser ()
operatorT' val = match' $
  \case
    OperatorT op
      | op == val -> Just ()
      | otherwise -> Nothing
    _            -> Nothing

iCommandT :: TokenParser (Text, [Text])
iCommandT = match' $
  \case
    ICommandT op args -> Just (op, args)
    _                 -> Nothing