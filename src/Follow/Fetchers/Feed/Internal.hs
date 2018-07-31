{-# LANGUAGE DataKinds #-}

module Follow.Fetchers.Feed.Internal
  ( getUrl
  , feedToEntries
  , getResponseBody
  , responseBodyToFeed
  ) where

import           Control.Applicative  ((<|>))
import           Control.Exception    (throwIO)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict)
import           Data.Dynamic         (Dynamic, fromDynamic)
import           Data.Maybe           (fromJust)
import           Follow.Types         (Entry (..), Recipe (..))
import qualified Network.HTTP.Req     as R (GET (..), MonadHttp, NoReqBody (..),
                                            Option, Scheme (..), Url,
                                            handleHttpException, lbsResponse,
                                            parseUrl, req, responseBody)
import           Text.Feed.Import     as FI (parseFeedSource)
import           Text.Feed.Query      as FQ (feedItems, getItemAuthor,
                                             getItemDescription, getItemId,
                                             getItemLink, getItemTitle)
import           Text.Feed.Types      as FT (Feed, Item)

type Url s = (R.Url s, R.Option s)

type EitherUrl = (Either (Url R.Http) (Url R.Https))

getUrl :: Recipe -> Maybe EitherUrl
getUrl recipe =
  (fromDynamic (dynamicUrl recipe) :: Maybe BS.ByteString) >>= R.parseUrl
  where
    dynamicUrl :: Recipe -> Dynamic
    dynamicUrl recipe = snd . head $ rArguments recipe

getResponseBody :: EitherUrl -> IO BL.ByteString
getResponseBody = either fetch fetch
  where
    fetch :: Url s -> IO BL.ByteString
    fetch (url, option) =
      R.responseBody <$> R.req R.GET url R.NoReqBody R.lbsResponse option

responseBodyToFeed :: BL.ByteString -> Maybe FT.Feed
responseBodyToFeed = FI.parseFeedSource

feedToEntries :: FT.Feed -> [Entry]
feedToEntries feed = itemToEntry <$> FQ.feedItems feed
  where
    itemToEntry :: FT.Item -> Entry
    itemToEntry item =
      Entry
        (fromJust $ FQ.getItemLink item)
        (snd . fromJust $ FQ.getItemId item)
        (FQ.getItemTitle item)
        (FQ.getItemDescription item)
        (FQ.getItemAuthor item)

instance R.MonadHttp IO where
  handleHttpException = throwIO
