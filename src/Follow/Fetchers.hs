{- |
Description: Top namespace to define fetcher strategies.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetched, Header, Result)

-- | Builds a directory from given header and entries
fetch :: Fetched -> Header -> Result Directory
fetch fetched header = Directory header <$> fetched
