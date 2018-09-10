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

import           Control.Monad.Catch           (MonadCatch, MonadThrow)
import qualified Data.ByteString               as BS (ByteString)
import           Follow.Fetchers.Feed.Internal
import           Follow.Types                  (Entry)
import           HTTP.Follow                   (getResponseBody, parseUrl)
import qualified Network.HTTP.Req              as R (MonadHttp)

-- | The fetcher strategy. Notice that it takes the URL as a
-- `BS.ByteString`, because URLs can't have characters outside of
-- ASCCI range, so they are `Word8` values.
fetch ::
     (R.MonadHttp m, MonadCatch m, MonadThrow m) => BS.ByteString -> m [Entry]
fetch url = do
  url' <- parseUrl url
  response <- getResponseBody url'
  feed <- parseFeed response
  return $ feedToEntries feed
