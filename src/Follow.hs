{- | Description: Top namespace for follow application.

Reexports main funtions to be performed at the application scope and
defines global operational functions.
-}
module Follow
  ( module Follow.Fetchers
  , module Follow.Digesters
  , process
  ) where

import           Follow.Digesters (digest)
import           Follow.Fetchers  (fetch)
import           Follow.Types     (Digester, Fetcher, Recipe, Result)

-- | Fetches and digests a recipe using given strategies
process :: Fetcher -> Digester a -> Recipe -> Result a
process fetcher digester recipe =
  let directory = fetch fetcher recipe
   in digest digester <$> directory
