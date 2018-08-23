{-|
Description: Strategy to fetch entries from a RSS or Atom feed.
-}
module Follow.Fetchers.Feed
  ( fetcher
  ) where

import           Control.Monad.Except          (liftEither, runExceptT)
import qualified Data.ByteString               as BS (ByteString)
import           Follow.Fetchers.Feed.Internal
import           Follow.Types                  (Fetcher)

-- | See `Follow.Types.Fetcher`.
fetcher :: Fetcher BS.ByteString
fetcher url = do
  url' <- liftEither $ parseUrl url
  response <- getResponseBody url'
  feed <- liftEither $ parseFeed response
  return $ feedToEntries feed
