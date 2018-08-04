{- |
Description: Top namespace to define fetcher strategies to fetch recipes.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetcher, Recipe)

fetch :: Recipe -> Fetcher -> IO (Either String Directory)
fetch recipe fetcher = fmap (Directory recipe) <$> fetcher recipe
