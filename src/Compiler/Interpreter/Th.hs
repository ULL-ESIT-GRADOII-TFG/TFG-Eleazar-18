{-# LANGUAGE TemplateHaskell #-}
module Compiler.Interpreter.Th where

import           Control.Monad
import           Control.Monad.Except
import           Data.Functor
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Text                     as T
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Parsec

import           Compiler.Interpreter
import           Compiler.Interpreter.Evaluate
import           Compiler.Prelude.Th
import           Compiler.Types

getIdentifiers :: String -> Either ParseError [String]
getIdentifiers code = (\par -> runParser par () "** Get Ids **" code) $ catMaybes <$> (many1 $ do
  let takeInner = do
        void $ string "${"
        inner <- many1 (satisfy (/= '}'))
        void $ string "}"
        return inner

  _ <- many1 (satisfy (/= '$'))
  optionMaybe takeInner)


rebuiltWithReplace :: String -> (String -> String) -> Either ParseError String
rebuiltWithReplace code replace = (\par -> runParser par () "** Rebuilt QQ **" code) $ join <$> (many1 $ do
  initial <- many1 (satisfy (/= '$'))
  mid <- choice $ map try
    [ do
        void $ string "${"
        inner <- many1 (satisfy (/= '}'))
        void $ string "}"
        return $ replace inner
    , string "$"
    , eof $> ""
    ]
  return $ initial ++ mid)

getSigE :: String -> Q Exp
getSigE name = do
  mName <- lookupValueName name
  case mName of
    Just name' -> do
      info <- reify name'
      case info of
        TyVarI _n t ->
          sigE (varE name') (return t)
        VarI _n t _ ->
          sigE (varE name') (return t)
        _ -> fail "Expect a VarI or TyVarI"
    Nothing -> fail $ "Name Not Found: " ++ name

showError :: Show a => Either a b -> Q b
showError (Right a)  = return a
showError (Left err) = fail $ "QQ Error: " ++ show err

quoteExpr :: String -> Q Exp
quoteExpr code = do
  names <- showError $ getIdentifiers code
  funcs <- mapM (normalize . getSigE) names
  [| do
    ids <- liftWorld $ sequence $(listE $ map (\expr -> [| (newVar . wrap $ ONative $(return expr) :: StWorld Address) |]) funcs)

    let replacer x = show $ fromJust $ M.lookup x (M.fromList (zip names (ids :: [Address])))
    case rebuiltWithReplace code replacer of
      Right code' -> compileSource (T.pack code') "** QQ Code **"
      Left err    -> throwError $ Parsing err
   |]


scriptflow :: QuasiQuoter
scriptflow = QuasiQuoter
  { quoteExp = quoteExpr
  , quotePat = fail "Not quote Pat"
  , quoteType = fail "Not quote Type"
  , quoteDec = fail "Not quote Dec "
  }
