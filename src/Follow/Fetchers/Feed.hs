{-|
Description: Defines a fetcher strategy to take entries from a RSS or
             Atom feed.

This module is the namespace to define a fetcher strategy which takes
entries from a URL pointing to a RSS or Atom feed.
-}
module Follow.Fetchers.Feed
  ( fetch
  , FeedError(..)
  ) where

import           Control.Monad.Catch           (MonadThrow)
import qualified Data.ByteString               as BS (ByteString)
import           Follow.Fetchers.Feed.Internal
import           Follow.Types                  (Fetched)
import           HTTP.Follow                   (getResponseBody, parseUrl)
import qualified Network.HTTP.Req              as R (MonadHttp)

-- | The fetcher strategy.
fetch :: (R.MonadHttp m, MonadThrow m) => BS.ByteString -> Fetched m
fetch url = do
  url' <- parseUrl url
  response <- getResponseBody url'
  feed <- parseFeed response
  return $ feedToEntries feed
