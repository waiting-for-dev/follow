{-# LANGUAGE DataKinds #-}

module Follow.Fetchers.Feed.Internal
  ( urlFromRecipe
  , feedToEntries
  , getResponseBody
  , parseFeed
  ) where

import           Control.Monad.Except (throwError)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Dynamic         (Dynamic, fromDynamic)
import           Data.Maybe           (fromJust)
import           Follow.Types         (Entry (..), FetchError (..),
                                       FetchFeedError (..), Recipe (..),
                                       Result (..))
import qualified Network.HTTP.Req     as R (GET (..), HttpException, MonadHttp,
                                            NoReqBody (..), Option, Scheme (..),
                                            Url, handleHttpException,
                                            lbsResponse, parseUrl, req,
                                            responseBody)
import           Text.Feed.Import     as F (parseFeedSource)
import           Text.Feed.Query      as F (feedItems, getItemAuthor,
                                            getItemDescription, getItemId,
                                            getItemLink, getItemTitle)
import           Text.Feed.Types      as F (Feed, Item)

type Url s = (R.Url s, R.Option s)

type EitherUrl = (Either (Url R.Http) (Url R.Https))

-- | Extract the URL from the arguments of a recipe
urlFromRecipe :: Recipe -> Either FetchError EitherUrl
urlFromRecipe recipe =
  case fromDynamic (dynamicUrl recipe) :: Maybe BS.ByteString of
    Nothing -> Left $ FetchFeedError URLFromDynamicConversionFailure
    Just value ->
      case R.parseUrl value of
        Nothing  -> Left $ FetchFeedError URLWrongFormat
        Just url -> Right url
  where
    dynamicUrl :: Recipe -> Dynamic
    dynamicUrl recipe = snd . head $ rArguments recipe

-- | Performs a request to given url and returns just the response body
getResponseBody :: EitherUrl -> Result BL.ByteString
getResponseBody = either fetch fetch
  where
    fetch :: Url s -> Result BL.ByteString
    fetch (url, option) =
      R.responseBody <$> R.req R.GET url R.NoReqBody R.lbsResponse option

-- | Parses a feed
parseFeed :: BL.ByteString -> Either FetchError F.Feed
parseFeed body =
  maybe (Left $ FetchFeedError FeedWrongFormat) Right $ F.parseFeedSource body

-- | Transforms a feed to a list of `Follow.Types.Entry`
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

-- | Declares how to handle request errors
instance R.MonadHttp Result where
  handleHttpException e = throwError $ FetchFeedError (ResponseError e)
