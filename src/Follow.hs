{- |
Description: Top namespace for follow application.

Reexports main funtions to be performed at the application scope.
-}
module Follow
  ( module Follow.Fetchers
  , module Follow.Digesters
  ) where

import           Follow.Digesters (digest)
import           Follow.Fetchers  (fetch)
