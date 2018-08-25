{-|
Description: Defines a fetcher strategy to take entries from a RSS or
             Atom feed.

This module is the namespace to define a fetcher strategy which takes
entries from a URL pointing to a RSS or Atom feed.
-}
module Follow.Fetchers.Feed
  ( fetch
  ) where

import           Control.Monad.Except          (liftEither, runExceptT)
import qualified Data.ByteString               as BS (ByteString)
import           Follow.Fetchers.Feed.Internal
import           Follow.Types                  (Fetcher)

-- | The fetcher strategy. Notice that it takes the URL as a
-- `BS.ByteString`, because URLs can't have characters outside of
-- ASCCI range, so they are `Word8` values.
fetch :: Fetcher BS.ByteString
fetch url = do
  url' <- liftEither $ parseUrl url
  response <- getResponseBody url'
  feed <- liftEither $ parseFeed response
  return $ feedToEntries feed
