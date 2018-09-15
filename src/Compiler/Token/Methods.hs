{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Compiler.Token.Methods where
  {-
    This file contains several functions extracted from alex Template. Several
    of them, are modified for this specific use
    Test:
    -- Compile with -funbox-strict-fields for best results!
  -}

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.ByteString.Internal   as ByteString (w2c)
import qualified Data.ByteString.Lazy       as ByteString
import           Data.Int                   (Int64)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Vector                as V
import           Lens.Micro.Platform

import {-# SOURCE #-} Compiler.Token.Lexer       (alexScan)
import           Compiler.Token.Types

-- -----------------------------------------------------------------------------
-- * Monad (with ByteString input)
alex_tab_size :: Int
alex_tab_size = 8

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,_,cs,n) =
  case ByteString.uncons cs of
    Nothing -> Nothing
    Just (b, cs') ->
      let c   = ByteString.w2c b
          p'  = alexMove p c
          n'  = n+1
      in p' `seq` cs' `seq` n' `seq` Just (b, (p', c, cs',n'))

alexStartPos :: TokPos
alexStartPos = TokPos 0 1 1

alexMove :: TokPos -> Char -> TokPos
alexMove (TokPos a l c) '\t' =
  TokPos (a+1) l (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (TokPos a l _) '\n' = TokPos (a+1) (l+1)   1
alexMove (TokPos a l c) _    = TokPos (a+1)  l     (c+1)

runAlex :: ByteString.ByteString -> TokenizerM a -> Either ErrorTokenizer a
runAlex input__ f = runExcept $ evalStateT f (
  TokenizerSt {
    alex_pos = alexStartPos,
    alex_bpos = 0,
    alex_inp = input__,
    alex_chr = '\n',
    alex_scd = 0,

    _stackGroups = [],
    _initialPos = alexStartPos,
    _indentStack = [],
    _generatedString = "",
    _kindGenString = None
  })

alexGetInput :: TokenizerM AlexInput
alexGetInput = do
  tokSt <- get
  let TokenizerSt{alex_pos=pos,alex_bpos=bpos,alex_chr=c,alex_inp=inp__} = tokSt
  return (pos,c,inp__,bpos)

alexSetInput :: AlexInput -> TokenizerM ()
alexSetInput (pos,c,inp__,bpos) = do
  modify (\s -> case s{alex_pos=pos,
                       alex_bpos=bpos,
                       alex_chr=c,
                       alex_inp=inp__} of
                  state__@TokenizerSt{} ->  state__)

-- | Improve errors
alexError :: T.Text -> TokenizerM a
alexError message = lift $ throwError (ErrorTokenizer message)

alexGetStartCode :: TokenizerM Int
alexGetStartCode = do
  tokSt <- get
  let TokenizerSt{alex_scd=sc} = tokSt
  return sc

alexSetStartCode :: Int -> TokenizerM ()
alexSetStartCode sc =
  modify (\s -> case s{alex_scd=sc} of
                  state__@TokenizerSt{} ->  state__)


alexMonadScan :: TokenizerM Lexeme
alexMonadScan = do
  inp__@(_,_,_,n) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError (tokPos,_,_,_) ->
      throwError $ ExpectedOtherToken tokPos []
    AlexSkip  inp__' _len -> do
      alexSetInput inp__'
      alexMonadScan
    AlexToken inp__'@(_,_,_,n') _ action -> do
      alexSetInput inp__'
      action inp__ len
      where
        len = n'-n

-- -----------------------------------------------------------------------------
-- Useful token actions


-- | just ignore this token and scan another one
skip :: AlexAction Lexeme
skip _input _len = alexMonadScan

-- | Ignore this token, but set the start code to a new value
begin :: Int -> AlexAction Lexeme
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- | perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

andAction :: AlexAction result -> AlexAction () -> AlexAction result
andAction action action2 input len = do
  value <- action input len
  _ <- action2 input len
  return value

-- | Get tokens from tokenizer
getTokens :: Tokenizer -> V.Vector Lexeme
getTokens (Partial toks)  = toks
getTokens (Complete toks) = toks

-- | Internal use.
alexEOF :: TokenizerM Lexeme
alexEOF = return (L (TokPos 0 0 0) (TokPos 0 0 0) EosT)

getInitialPosSizeOffset :: TokPos -> Int64 -> TokPos
getInitialPosSizeOffset (TokPos offset line column) len = TokPos (offset - (fromIntegral len)) line (column - (fromIntegral len))

-- | Internal use.
mkL :: Token -> AlexInput -> Int64 -> TokenizerM Lexeme
mkL c (p,_,_,_) len = return (L (getInitialPosSizeOffset p len) p c)

-- | Start a group of pairs
mkGroupOL :: Token -> AlexInput -> Int64 -> TokenizerM Lexeme
mkGroupOL c (p,_,_,_) len = do
  stackGroupsA %= ((c,p):)
  return (L (getInitialPosSizeOffset p len) p c)

-- | Close a group a pairs the first `Token` marks which should be poped
mkGroupCL :: Token -> Token -> AlexInput -> Int64 -> TokenizerM Lexeme
mkGroupCL pop c (p,_,_,_) len = do
  stack <- use stackGroupsA
  case stack of
    [] ->
      throwError $ WrongClosedGroup
        { expected = pop, found = Nothing, closing = (c, p)}
    ((lastTk, lastPos):rest)
      | lastTk == pop -> do
          stackGroupsA .= rest
          return (L (getInitialPosSizeOffset p len) p c)
      | otherwise ->
        throwError $ WrongClosedGroup
          { expected = pop, found = Just (lastTk, lastPos), closing = (c, p) }


-- | Internal use.
mkL' :: (T.Text -> Token) -> AlexInput -> Int64 -> TokenizerM Lexeme
mkL' mkTok (p,_,str,_) len =
  return (L (getInitialPosSizeOffset p len) p (mkTok . T.decodeUtf8 . ByteString.toStrict $ ByteString.take len str))

-- | Starts a string tokenizer, TODO:Add token position
kindString :: KindString -> AlexInput -> Int64 -> TokenizerM ()
kindString kind (p,_,_,_) _len = do
  initialPosA .= p
  kindGenStringA .= kind

kindString' :: KindString -> Int -> AlexInput -> Int64 -> TokenizerM Lexeme
kindString' kind code input len = do
  kindGenStringA .= kind
  begin code input len

-- | Remove delimitators from string
skipDels :: T.Text -> T.Text
skipDels = T.dropEnd 1 . T.drop 1

-- | Fill internal string
addToInnerString :: AlexInput -> Int64 -> TokenizerM Lexeme
addToInnerString input@(_,_,str,_) len = do
  generatedStringA %= (<> T.decodeUtf8 (ByteString.toStrict (ByteString.take len str)))
  mkL SkipT input len

-- | Replace raw value with `val` entered
skipJustAdd :: T.Text -> AlexInput -> Int64 -> TokenizerM Lexeme
skipJustAdd val input len = do
  generatedStringA %= (<> val)
  mkL SkipT input len

-- | Generate a string like token.
generateLexerFromInner :: (T.Text -> Token) -> AlexInput -> Int64 -> TokenizerM Lexeme
generateLexerFromInner mkTok (p,_,_,_) _ = do
  str <- use generatedStringA
  initialPos <- use initialPosA
  generatedStringA .= ""
  kindGenStringA .= None
  return (L initialPos p (mkTok str))

-- | Internal use. Generate a new ident in the tokenizer
newIndent :: AlexInput -> Int64 -> TokenizerM Lexeme
newIndent input len = do
  indentStackA %= (len:)
  mkL SkipT input len

-- | Internal use. Check to add correspond token to decrement
checkDecrement :: AlexInput -> Int64 -> TokenizerM Lexeme
checkDecrement input len = do
  indentStack <- use indentStackA
  case indentStack of
    []
      | 0 == len -> mkL EndStmtT input len
      | otherwise -> mkL SkipT input len
    stack@(x:_)
      | x == len  -> mkL EndStmtT input len
      | x < len   -> mkL SkipT input len
      | otherwise -> do -- getDedent len stack
          let (removes, rest) = break (<= len) stack
          indentStackA .= rest
          mkL (DedentT $ length removes) input len

-- | Run tokenizer over string
scanner
  :: Bool -- ^ Use to close indentation, by default it should be true. (Used in
          --   interpreter to check multiline input)
   -> ByteString.ByteString -- ^ Source text code
   -> Either ErrorTokenizer Tokenizer
scanner autoClose str = runAlex str $ do
  let loop vec = do
        lexeme@(L s e tok) <- alexMonadScan
        case tok of
          SkipT -> loop vec
          DedentT n -> loop (vec `mappend` V.replicate n (L s e CBraceT))
          EosT -> do
            userState <- get
            complement <- case userState ^. kindGenStringA of
                            None -> return Nothing
                            Shell -> fmap Just $ generateLexerFromInner ShellCommandT (alex_pos userState, alex_chr userState, alex_inp userState, alex_bpos userState) 0
                            _ -> return Nothing
                            -- Str -> generateLexerFromInner LitTextT
                            -- Reg -> generateLexerFromInner RegexExprT
            let vec' = maybe vec (vec `V.snoc`) complement
            if autoClose then do
              let n = length $ userState ^. indentStackA
              return (vec' `mappend` V.replicate n (L s e CBraceT))
            else
              return vec'
          _ -> loop (vec `V.snoc` lexeme)
  toks <- loop mempty
  kindGenString <- use kindGenStringA
  stackGroups <- use stackGroupsA
  if V.null toks then
    return $ Complete toks
  else if (tokn (V.last toks) == OBraceT)
          && (kindGenString /= None)
          && (kindGenString /= Shell)
          && (not $ null stackGroups) then
    return $ Partial toks
  else
    return $ Complete toks
