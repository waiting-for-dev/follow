{-|
Description: Wiring for the feed fecther strategy.

This module defains low level helper functions to be used by the feed
fetcher strategy.
-}
module Follow.Fetchers.Feed.Internal
  ( feedToEntries
  , parseFeed
  ) where

import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Time            (UTCTime)
import           Data.Time.Follow     (parseTimeGuess)
import           Follow.Types         (Entry (..), FetchError (..),
                                       FetchFeedError (..), Result (..))
import           Text.Feed.Import     as F (parseFeedSource)
import           Text.Feed.Query      as F (feedItems, getItemAuthor,
                                            getItemDescription, getItemId,
                                            getItemLink,
                                            getItemPublishDateString,
                                            getItemTitle)
import           Text.Feed.Types      as F (Feed, Item)

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
        (parseTimeGuess =<< F.getItemPublishDateString item)
