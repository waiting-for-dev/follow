{- |
Description: Top namespace to define fetcher strategies to fetch recipes.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetcher, Recipe, Result)

fetch :: Recipe -> Fetcher -> Result Directory
fetch recipe fetcher = Directory recipe <$> fetcher recipe
