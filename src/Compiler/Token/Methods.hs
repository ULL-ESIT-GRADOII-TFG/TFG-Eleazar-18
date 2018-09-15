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

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- -----------------------------------------------------------------------------
-- * Monad (with ByteString input)

runAlex :: ByteString.ByteString -> TokenizerM a -> Either ErrorTokenizer a
runAlex input__ f = runExcept $ evalStateT f (
  TokenizerSt {
    alex_pos = alexStartPos,
    alex_bpos = 0,
    alex_inp = input__,
    alex_chr = '\n',
    alex_scd = 0,

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
                  state__@(TokenizerSt{}) ->  state__)

-- | Improve errors
alexError :: String -> TokenizerM a
alexError _message = lift $ throwError ErrorTokenizer

alexGetStartCode :: TokenizerM Int
alexGetStartCode = do
  tokSt <- get
  let TokenizerSt{alex_scd=sc} = tokSt
  return sc

alexSetStartCode :: Int -> TokenizerM ()
alexSetStartCode sc =
  modify (\s -> case s{alex_scd=sc} of
                  state__@(TokenizerSt{}) ->  state__)


alexMonadScan :: TokenizerM Lexeme
alexMonadScan = do
  inp__@(_,_,_,n) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
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

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

-- token :: (AlexInput -> Int64 -> token) -> AlexAction token
-- token t input__ len = return (t input__ len)


-- | Get tokens from tokenizer
getTokens :: Tokenizer -> V.Vector Lexeme
getTokens (Partial toks)  = toks
getTokens (Complete toks) = toks

-- | Internal use.
alexEOF :: TokenizerM Lexeme
alexEOF = return (L (AlexPn 0 0 0) (AlexPn 0 0 0) EosT)

-- | Internal use.
mkL :: Token -> AlexInput -> Int64 -> TokenizerM Lexeme
mkL c (p,_,_,_) _ = return (L p p c)

-- | Internal use.
mkL' :: (T.Text -> Token) -> AlexInput -> Int64 -> TokenizerM Lexeme
mkL' mkTok (p,_,str,_) len =
  return (L p p (mkTok . T.decodeUtf8 . ByteString.toStrict $ ByteString.take len str))

-- TODO: By here
-- beginKindString :: KindString -> AlexInput -> Int -> TokenizerM Lexeme
-- beginKindString' kind (p,_,_,str) len = do
--   get
--   return (L p (mkTok . T.pack $ take len str))


-- | Remove delimitators from string
skipDels :: T.Text -> T.Text
skipDels = T.dropEnd 1 . T.drop 1

-- | FIX: Position token
addToInnerString :: AlexInput -> Int64 -> TokenizerM Lexeme
addToInnerString input@(_,_,str,_) len = do
  generatedStringA %= (<> T.decodeUtf8 (ByteString.toStrict (ByteString.take len str)))
  mkL SkipT input len

skipJustAdd :: T.Text -> AlexInput -> Int64 -> TokenizerM Lexeme
skipJustAdd val input len = do
  generatedStringA %= (<> val)
  mkL SkipT input len

generateLexerFromInner :: (T.Text -> Token) -> AlexInput -> Int64 -> TokenizerM Lexeme
generateLexerFromInner mkTok input len = do
  str <- use generatedStringA
  generatedStringA .= ""
  mkL (mkTok str) input len

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
            -- complement <- case userState ^. kindGenStringA of
            --                 None -> return []
            --                 Shell -> fmap (:[]) $ generateLexerFromInner ShellCommandT
            --                 _ -> return []
            --                 -- Str -> generateLexerFromInner LitTextT
            --                 -- Reg -> generateLexerFromInner RegexExprT
            -- let vec' = vec `V.snoc` complement
            if autoClose then do
              let n = length $ userState ^. indentStackA
              return (vec `mappend` V.replicate n (L s e CBraceT))
            else
              return vec
          _ -> loop (vec `V.snoc` lexeme)
  toks <- loop mempty
  kindGenString <- use kindGenStringA
  if V.null toks then
    return $ Complete toks
  else if (tokn (V.last toks) == OBraceT)
          && (kindGenString /= None)
          && (kindGenString /= Shell) then -- TODO: Add more Partial cases strings, apply...
    return $ Partial toks
  else
    return $ Complete toks
