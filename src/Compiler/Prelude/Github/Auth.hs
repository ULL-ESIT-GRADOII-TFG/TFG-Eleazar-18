{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Compiler.Prelude.Github.Auth where

import           Control.Monad.IO.Class
import qualified Data.Aeson              as A
import qualified Data.ByteString.Char8   as BS8
import qualified Data.Text               as T
-- import qualified Data.Text.Encoding         as T
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Compiler.Types

{- |
Github API: https://developer.github.com/v3/oauth_authorizations/
-}

data ResponseToken = ResponseToken
  { id               :: Int
  , url              :: T.Text
  , app              :: ResponseApp
  , token            :: T.Text
  , hashed_token     :: T.Text
  , token_last_eight :: T.Text
  , note             :: T.Text
  , note_url         :: Maybe T.Text
  , updated_at       :: T.Text
  , created_at       :: T.Text
  , scopes           :: [T.Text]
  , finger_print     :: Maybe T.Text
  } deriving (Generic, Show)

instance A.ToJSON ResponseToken where
      toEncoding = A.genericToEncoding A.defaultOptions
instance A.FromJSON ResponseToken

data ResponseApp = ResponseApp
  { name      :: T.Text
  , url       :: T.Text
  , client_id :: T.Text
  } deriving (Show, Generic)

instance A.ToJSON ResponseApp where
      toEncoding = A.genericToEncoding A.defaultOptions
instance A.FromJSON ResponseApp

data RequestAddAuth = RequestAddAuth
  { scopes :: [T.Text]
  , note   :: T.Text
  } deriving (Show, Generic)

instance A.ToJSON RequestAddAuth where
      toEncoding = A.genericToEncoding A.defaultOptions
instance A.FromJSON RequestAddAuth

-- | Return true if the operations was sucessful
requestLogin :: String -> String -> StWorld Bool
requestLogin user password = do
  let requestAuth = RequestAddAuth ["user", "repo", "notifications", "gist"] "GHScript"

  manager <- liftIO $ newManager tlsManagerSettings
  let Just initialRequest = parseRequest "https://api.github.com/authorizations"
  -- logDebugN (TL.toStrict . TL.decodeUtf8 $ A.encode requestAuth)
  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS (A.encode requestAuth)
        , requestHeaders = [ ("User-Agent", "GHScript") ]
        }

  let request' = applyBasicAuth (BS8.pack user) (BS8.pack password) request
  --logDebugN "Making HTTP call"
  response <- liftIO $ httpLbs request' manager
  --logDebugN ((TL.toStrict . TL.decodeUtf8 . responseBody) response)
  case A.decode $ responseBody response :: Maybe ResponseToken of
    Just _resp -> do
    --   programState.auth .= (Just . OAuth . T.encodeUtf8 $ token resp)
    --   programState.userName .= (Just $ T.pack user)
    --   saveConfigFile
      return True
    Nothing -> liftIO $ putStrLn "Fail on authentication" >> return False
