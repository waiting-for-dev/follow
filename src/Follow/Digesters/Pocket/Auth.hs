{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Authentication logic for Pocket.

This module contains functions to deal with the authentication process
of Pocket. Read https://getpocket.com/developer/docs/authentication to
have all the details of this process.

A user friendly method to interact with the authentication process is
through the command:

@
stack exec follow_pocket_auth
@
-}
module Follow.Digesters.Pocket.Auth
  ( requestTokenStep
  , accessTokenStep
  , jsonPostResponseBody
  , jsonHeaders
  , consumerKey
  ) where

import           Control.Monad.Except (throwError)
import           Data.Aeson           (ToJSON, Value (..), object, (.=))
import qualified Data.HashMap.Strict  as HS
import           Data.Text            (Text)
import qualified Data.Text            as T (concat)
import           Follow.Types         (FetchError (..), Result)
import           GHC.Generics         (Generic)
import           HTTP.Follow
import           Network.HTTP.Req     (Option, POST (..), ReqBodyJson (..), Url,
                                       header, https, jsonResponse, req,
                                       responseBody, (/:))

-- | Consumer key for the Follow application in Pocket.
consumerKey :: Text
consumerKey = "80296-6e350545b4382d839b3aa5df"

-- | First auth step. Asks Pocket for a request token. It returns the
-- token itself and the URL that the user must visit in order to grant
-- access before proceeding to the second step.
requestTokenStep :: Result (Text, Text)
requestTokenStep = do
  token <- getRequestToken
  processTokenStep token $ \token ->
    ( token
    , T.concat
        [ "https://getpocket.com/auth/authorize?request_token="
        , token
        , "&redirect_uri="
        , "https://getpocket.com"
        ])

-- | Second auth step. Once a user has granted permission to Follow,
-- it exchanges a request token for an access token. The access token
-- is what it needs to be used in any other Pocket API call.
accessTokenStep :: Text -> Result Text
accessTokenStep rToken = do
  token <- getAccessToken rToken
  processTokenStep token id

processTokenStep :: Maybe Value -> (Text -> a) -> Result a
processTokenStep token f =
  case token of
    Nothing             -> throwError TokenNotFound
    Just (String token) -> return $ f token
    Just _              -> throwError TokenDecodingError

-- | Gets the JSON response body of a POST request.
jsonPostResponseBody ::
     ToJSON b
  => Url scheme
  -> b
  -> Option scheme
  -> Follow.Types.Result (HS.HashMap Text Value)
jsonPostResponseBody url body options =
  responseBody <$> req POST url (ReqBodyJson body) jsonResponse options

-- | Pocket expected headers for a JSON interaction.
jsonHeaders =
  header "Content-Type" "application/json; charset=UTF-8" <>
  header "X-Accept" "application/json"

getRequestToken :: Result (Maybe Value)
getRequestToken =
  parseValue "code" <$> jsonPostResponseBody url body jsonHeaders
  where
    url = https "getpocket.com" /: "v3" /: "oauth" /: "request"
    body =
      object
        [ "consumer_key" .= String consumerKey
        , "redirect_uri" .= String "https://getpocket.com"
        ]

getAccessToken :: Text -> Follow.Types.Result (Maybe Value)
getAccessToken rToken =
  parseValue "access_token" <$> jsonPostResponseBody url body jsonHeaders
  where
    url = https "getpocket.com" /: "v3" /: "oauth" /: "authorize"
    body = object ["consumer_key" .= String consumerKey, "code" .= rToken]

parseValue :: Text -> HS.HashMap Text Value -> Maybe Value
parseValue = HS.lookup
