{- |
Description: Top namespace to define fetcher strategies and global
             fetching functions.

This module contains functions involved in the fetching process. It
also serves as a top namespace for other modules defining concrete
fetcher strategies.
-}
module Follow.Fetchers
  ( buildDirectory
  ) where

import           Follow.Types (Directory (..), Fetched, Result, Subject)

-- | Helper to build a directory from a subject and a list of fetched
-- entries.
buildDirectory :: Fetched -> Subject -> Result Directory
buildDirectory fetched header = Directory header <$> fetched
