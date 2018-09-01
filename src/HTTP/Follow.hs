{-# LANGUAGE DataKinds #-}

{-|
Description: HTTP utils used elsewhere in the library.

This module contains HTTP functions needed and used from other modules
within the `Follow` library.
-}
module HTTP.Follow
  ( parseUrl
  , getResponseBody
  ) where

import           Control.Monad.Except (throwError)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Follow.Types         (FetchError (..), Result (..))
import qualified Network.HTTP.Req     as R (GET (..), HttpException, MonadHttp,
                                            NoReqBody (..), Option, Scheme (..),
                                            Url, handleHttpException,
                                            lbsResponse, parseUrl, req,
                                            responseBody)

type Url s = (R.Url s, R.Option s)

type EitherUrl = (Either (Url R.Http) (Url R.Https))

-- | Parses a url type from a textual representation.
parseUrl :: BS.ByteString -> Either FetchError EitherUrl
parseUrl url =
  case R.parseUrl url of
    Nothing   -> Left URLWrongFormat
    Just url' -> Right url'

-- | Performs a request to given url and returns just the response body
getResponseBody :: EitherUrl -> Result BL.ByteString
getResponseBody = either fetch fetch
  where
    fetch :: Url s -> Result BL.ByteString
    fetch (url, option) =
      R.responseBody <$> R.req R.GET url R.NoReqBody R.lbsResponse option

-- | Declares how to handle request errors.
instance R.MonadHttp Result where
  handleHttpException e = throwError $ ResponseError e
