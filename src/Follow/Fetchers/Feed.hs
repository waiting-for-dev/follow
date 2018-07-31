module Follow.Fetchers.Feed
  ( argumentsDSL
  , fetcher
  ) where

import           Data.Dynamic                  (toDyn)
import           Follow.DSL.Format             (uriFormat)
import           Follow.Fetchers.Feed.Internal
import           Follow.Types                  (ArgumentsDSL, Fetcher)

argumentsDSL :: ArgumentsDSL
argumentsDSL = [("URL", toDyn <$> uriFormat)]

fetcher :: Fetcher
fetcher recipe =
  case getUrl recipe of
    Nothing -> return Nothing
    Just url ->
      fmap feedToEntries <$> (responseBodyToFeed <$> getResponseBody url)
