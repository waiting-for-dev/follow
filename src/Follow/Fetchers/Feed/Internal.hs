{-# LANGUAGE DeriveAnyClass #-}

{-|
Description: Wiring for the feed fecther strategy.

This module defains low level helper functions to be used by the feed
fetcher strategy.
-}
module Follow.Fetchers.Feed.Internal
  ( feedToEntries
  , parseFeed
  , FeedError(..)
  ) where

import           Control.Monad.Catch  (Exception, MonadCatch, MonadThrow,
                                       throwM)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Time            (UTCTime)
import           Data.Time.Follow     (parseTimeGuess)
import           Follow.Types         (Entry (..))
import           Text.Feed.Import     as F (parseFeedSource)
import           Text.Feed.Query      as F (feedItems, getItemAuthor,
                                            getItemDescription, getItemId,
                                            getItemLink,
                                            getItemPublishDateString,
                                            getItemTitle)
import           Text.Feed.Types      as F (Feed, Item)

-- | Error when parsing a feed
data FeedError =
  FeedWrongFormat
  deriving (Show, Eq, Exception)

-- | Parses a feed type from a textual representation.
parseFeed :: (MonadThrow m, MonadCatch m) => BL.ByteString -> m F.Feed
parseFeed body = maybe (throwM FeedWrongFormat) return (F.parseFeedSource body)

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
