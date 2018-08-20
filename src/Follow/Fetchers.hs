{- |
Description: Top namespace to define fetcher strategies to fetch recipes.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetcher, Recipe, Result)

fetch :: Fetcher -> Recipe -> Result Directory
fetch fetcher recipe = Directory recipe <$> fetcher recipe
