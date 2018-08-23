{- |
Description: Top namespace to define fetcher strategies to fetch recipes.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetched, Recipe, Result)

-- | Builds a directory from given recipe using given fetcher
fetch :: Fetched -> Recipe -> Result Directory
fetch fetched recipe = Directory recipe <$> fetched
