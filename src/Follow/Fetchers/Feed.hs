{-|
Description: Strategy to fetch entries from a RSS or Atom feed.
-}
module Follow.Fetchers.Feed
  ( argumentsDSL
  , fetcher
  ) where

import           Control.Monad.Except          (liftEither, runExceptT)
import           Data.Dynamic                  (toDyn)
import           Follow.DSL.Format             (uriFormat)
import           Follow.Fetchers.Feed.Internal
import           Follow.Types                  (ArgumentsDSL, Fetcher)

-- | See `Follow.Types.ArgumentsDSL`.
argumentsDSL :: ArgumentsDSL
argumentsDSL = [("URL", toDyn <$> uriFormat)]

-- | See `Follow.Types.Fetcher`.
fetcher :: Fetcher
fetcher recipe = do
  url <- liftEither $ urlFromRecipe recipe
  response <- getResponseBody url
  feed <- liftEither $ parseFeed response
  return $ feedToEntries feed
