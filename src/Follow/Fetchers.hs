{- |
Description: Top namespace to define fetcher strategies to fetch recipes.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetcher, Recipe, Result)

-- | Builds a directory from given recipe using given fetcher
fetch :: Fetcher -> Recipe -> Result Directory
fetch fetcher recipe = Directory recipe <$> fetcher recipe
