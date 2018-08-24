{- |
Description: Top namespace to define fetcher strategies.
-}
module Follow.Fetchers where

import           Follow.Types (Directory (..), Fetched, Result, Subject)

-- | Builds a directory from given header and entries
fetch :: Fetched -> Subject -> Result Directory
fetch fetched header = Directory header <$> fetched
