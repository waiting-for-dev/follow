{-# LANGUAGE DataKinds #-}

{-|
Description: Wiring for the feed fecther strategy.

This module defains low level helper functions to be used by the feed
fetcher strategy.
-}
module Follow.Fetchers.Feed.Internal
  ( parseUrl
  , feedToEntries
  , getResponseBody
  , parseFeed
  ) where

import           Control.Monad        (join)
import           Control.Monad.Except (throwError)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Time            (UTCTime)
import           Follow.Types         (Entry (..), FetchError (..),
                                       FetchFeedError (..), Result (..))
import qualified Network.HTTP.Req     as R (GET (..), HttpException, MonadHttp,
                                            NoReqBody (..), Option, Scheme (..),
                                            Url, handleHttpException,
                                            lbsResponse, parseUrl, req,
                                            responseBody)
import           Text.Feed.Import     as F (parseFeedSource)
import           Text.Feed.Query      as F (feedItems, getItemAuthor,
                                            getItemDescription, getItemId,
                                            getItemLink, getItemPublishDate,
                                            getItemTitle)
import           Text.Feed.Types      as F (Feed, Item)

type Url s = (R.Url s, R.Option s)

type EitherUrl = (Either (Url R.Http) (Url R.Https))

-- | Parses a url type from a textual representation.
parseUrl :: BS.ByteString -> Either FetchError EitherUrl
parseUrl url =
  case R.parseUrl url of
    Nothing   -> Left $ FetchFeedError URLWrongFormat
    Just url' -> Right url'

-- | Performs a request to given url and returns just the response body
getResponseBody :: EitherUrl -> Result BL.ByteString
getResponseBody = either fetch fetch
  where
    fetch :: Url s -> Result BL.ByteString
    fetch (url, option) =
      R.responseBody <$> R.req R.GET url R.NoReqBody R.lbsResponse option

-- | Parses a feed type from a textual representation.
parseFeed :: BL.ByteString -> Either FetchError F.Feed
parseFeed body =
  maybe (Left $ FetchFeedError FeedWrongFormat) Right $ F.parseFeedSource body

-- | Transforms a feed to a list of entries.
feedToEntries :: F.Feed -> [Entry]
feedToEntries feed = itemToEntry <$> F.feedItems feed
  where
    itemToEntry :: F.Item -> Entry
    itemToEntry item =
      Entry
        (F.getItemLink item)
        (snd <$> F.getItemId item)
        (F.getItemTitle item)
        (F.getItemDescription item)
        (F.getItemAuthor item)
        (join $ F.getItemPublishDate item)

-- | Declares how to handle request errors.
instance R.MonadHttp Result where
  handleHttpException e = throwError $ FetchFeedError (ResponseError e)
