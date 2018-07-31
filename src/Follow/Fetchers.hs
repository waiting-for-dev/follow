{- |
Description: Top namespace to define fetcher strategies to fetch recipes.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetcher, Recipe)

fetch :: Recipe -> Fetcher -> IO (Maybe Directory)
fetch recipe fetcher = do
  entries <- fetcher recipe
  case entries of
    Nothing -> return Nothing
    Just xs -> return (Just $ Directory recipe xs)
