{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Send entries in the directory to Pocket.

This module contains the definition of a digester which sends all the
entries within the directory to Pocket (https://getpocket.com)
application.

In order to be able to send the information to Pocket, first the user
needs to authorize Follow application. For this reason, this module
also exports functions which deal with the authentication
process. However, as a user, the most straighforward way to
authenticate is to execute:

@
stack exec follow_pocket_auth
@

After the process is finished, the previous command will print an
access token which needs to be provided to the `digest` method.

-}
module Follow.Digesters.Pocket
  ( requestTokenStep
  , accessTokenStep
  , digest
  ) where

import           Control.Monad.Catch          (MonadThrow)
import           Data.Aeson                   (Value, object, (.=))
import qualified Data.HashMap.Strict          as HS
import           Data.Maybe                   (fromJust, isJust)
import           Data.Text                    (Text)
import           Follow.Digesters.Pocket.Auth
import           Follow.Types                 (Digester, Directory (..),
                                               Entry (..))
import           Network.HTTP.Req             (MonadHttp, https, (/:))

digest ::
     (MonadHttp m, MonadThrow m) => Text -> Digester (m (HS.HashMap Text Value))
digest token directory = jsonPostResponseBody url body jsonHeaders
  where
    url = https "getpocket.com" /: "v3" /: "send"
    body =
      object
        [ "consumer_key" .= consumerKey
        , "access_token" .= token
        , "actions" .=
          (entryToAction <$> filter (isJust . eURI) (dEntries directory))
        ]
    entryToAction entry =
      object
        [ "action" .= ("add" :: Text)
        , "url" .= ((fromJust $ eURI entry) :: Text)
        ]
