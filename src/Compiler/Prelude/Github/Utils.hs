{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Compiler.Prelude.Github.Utils where

import           Control.Monad.State.Strict
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BS8
import           Data.String
import qualified Data.Text                  as T
import           Lens.Micro.Platform
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Compiler.Types
import           Compiler.Utils


data GState = GState
  { _manager  :: Manager
  , _username :: T.Text
  , _token    :: BS8.ByteString
  }

makeSuffixLenses ''GState

type GithubSt a = StateT GState StWorld a

initGH :: GithubSt ()
initGH = do
  manager <- lift $ liftIO $ newManager tlsManagerSettings
  managerA .= manager

requestWith :: (A.ToJSON a, A.FromJSON b) => String -> String -> a -> GithubSt (Maybe b)
requestWith urlReq methodReq body = do
  manager <- use managerA
  token <- use tokenA
  lift $ do
    let Just initialRequest = parseRequest urlReq
    let request = initialRequest
          { method = fromString methodReq
          , requestBody = RequestBodyLBS (A.encode body)
          , requestHeaders =
            [ ("User-Agent", "ScriptFlow")
            , ("Authorization", "token " <> token)] -- TODO: It should be configurable
          }

    response <- liftIO $ httpLbs request manager
    return $ A.decode $ responseBody response
