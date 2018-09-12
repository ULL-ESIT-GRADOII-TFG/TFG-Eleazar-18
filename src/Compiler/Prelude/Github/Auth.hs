{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Compiler.Prelude.Github.Auth where

import           Control.Monad.Trans
import qualified Data.Aeson                    as A
import qualified Data.ByteString.Char8         as BS8
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           GHC.Generics
import           Lens.Micro.Platform
import           Network.HTTP.Client

import           Compiler.Prelude.Github.Utils

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
requestLogin :: String -> String -> GithubSt Bool
requestLogin user password = do
  manager <- use managerA
  let requestAuth = RequestAddAuth ["user", "repo", "notifications", "gist"] "ScriptFlow"
  let Just initialRequest = parseRequest "https://api.github.com/authorizations"
  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS (A.encode requestAuth)
        , requestHeaders = [ ("User-Agent", "ScriptFlow") ]
        }
  mResp <- lift $ do
    let request' = applyBasicAuth (BS8.pack user) (BS8.pack password) request
    response <- liftIO $ httpLbs request' manager
    return (A.decode $ responseBody response :: Maybe ResponseToken)

  case mResp of
    Just resp -> do
      tokenA .= T.encodeUtf8 (token resp)
      usernameA .= T.pack user
      return True
    Nothing -> liftIO $ putStrLn "Fail on authentication" >> return False


{-
[|
 GET /user/:username/repos:
   params:
    - name: "type"
      type: string
      default: 'owner'
      values: ['all', 'owner', 'member']
    - name: "sort"
      type: string
      default: 'full_name'
      values: ['all', 'updated', 'pushed', 'full_name']
    - name: "sort"
      type: string
      default: 'full_name'
      values: ['all', 'updated', 'pushed', 'full_name']
|]
-}
