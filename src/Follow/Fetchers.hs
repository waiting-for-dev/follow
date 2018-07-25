{- |
Description: Top namespace to define fetcher strategies to fetch recipes.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetcher, Recipe)

fetch :: Recipe -> Fetcher -> IO Directory
fetch recipe fetcher =
  fetcher recipe >>= \entries -> return $ Directory recipe entries
