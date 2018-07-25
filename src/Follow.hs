{- |
Description: Top namespace for follow application.
-}
module Follow
  ( fetchContent
  ) where

import           Follow.Types (Directory (..), Fetcher, Recipe)

fetchContent :: Recipe -> Fetcher -> Directory
fetchContent recipe fetcher =
  let urls = fetcher recipe
   in Directory recipe urls
