{- |
Description: Top namespace for follow application.

Reexports main funtions to be performed at the application scope.
-}
module Follow
  ( module Follow.Fetchers
  ) where

import           Follow.Fetchers (fetch)
