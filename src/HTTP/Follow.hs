{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Description: HTTP utils used elsewhere in the library.

This module contains HTTP functions needed and used from other modules
within the `Follow` library.
-}
module HTTP.Follow
  ( parseUrl
  , getResponseBody
  , HTTPError(..)
  ) where

import           Control.Monad.Catch  (Exception, MonadCatch, MonadThrow,
                                       throwM)
import           Control.Monad.Except (throwError)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Network.HTTP.Req     as R (GET (..), HttpException, MonadHttp,
                                            NoReqBody (..), Option, Scheme (..),
                                            Url, handleHttpException,
                                            lbsResponse, parseUrl, req,
                                            responseBody)

type Url s = (R.Url s, R.Option s)

type EitherUrl = (Either (Url R.Http) (Url R.Https))

-- HTTP errors
data HTTPError =
  URLWrongFormat
  deriving (Eq, Show, Exception)

-- | Parses a url type from a textual representation.
parseUrl :: (MonadCatch m, MonadThrow m) => BS.ByteString -> m EitherUrl
parseUrl url = maybe (throwM URLWrongFormat) return (R.parseUrl url)

-- | Performs a request to given url and returns just the response body
getResponseBody ::
     (R.MonadHttp m, MonadCatch m, MonadThrow m) => EitherUrl -> m BL.ByteString
getResponseBody = either fetch fetch
  where
    fetch (url, option) =
      R.responseBody <$> R.req R.GET url R.NoReqBody R.lbsResponse option

-- | Declares how to handle request errors.
instance R.MonadHttp IO where
  handleHttpException = throwM
