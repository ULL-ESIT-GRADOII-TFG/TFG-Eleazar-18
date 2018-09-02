{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Compiler.Prettify
  ( putDocLnPP
  , putDocPP
  , pattern NoVerbose
  , pattern LowVerbose
  , pattern ASTOutput
  , pattern ASTLoc
  )
where

import           Control.Monad
import qualified Data.Text                                   as T
import qualified Data.Text.IO                                as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Util.Panic
import           System.IO

type Verbosity = Int

pattern NoVerbose :: Int
pattern NoVerbose = 0

pattern LowVerbose :: Int
pattern LowVerbose = 1

pattern ASTOutput :: Int
pattern ASTOutput = 3

pattern ASTLoc :: Int
pattern ASTLoc = 4

renderIO' :: Verbosity -> Handle -> SimpleDocStream Verbosity -> IO ()
renderIO' verbose h = go False
  where
    go :: Bool -> SimpleDocStream Verbosity -> IO ()
    go skip = \case
        SFail              -> panicUncaughtFail
        SEmpty             -> pure ()
        SChar c rest       -> do
          when (not skip) $
            hPutChar h c
          go skip rest
        SText _ t rest     -> do
          when (not skip) $
            T.hPutStr h t
          go skip rest
        SLine n rest       -> do
          when (not skip) $ do
            hPutChar h '\n'
            T.hPutStr h (T.replicate n " ")
          go skip rest
        SAnnPush verbosity rest ->
          if verbosity > verbose then
            go True rest
          else
            go False rest
        SAnnPop rest       -> go False rest

putDocPP :: Verbosity -> Doc Verbosity -> IO ()
putDocPP verbose = hPutDoc' verbose stdout

putDocLnPP :: Verbosity -> Doc Verbosity -> IO ()
putDocLnPP verbose doc = hPutDoc' verbose stdout (doc <> line)

hPutDoc' :: Verbosity -> Handle -> Doc Verbosity -> IO ()
hPutDoc' verbose h doc = renderIO' verbose h (layoutPretty defaultLayoutOptions doc)
