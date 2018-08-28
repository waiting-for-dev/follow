{-|
Description: Middleware to sort entries according to a function.

This middleware allows to sort the directory entries according to a
comparison function. The comparison function has a type @Entry ->
Entry -> Ordering@.

Some pre-built comparison function builders are also
exported. Example:

@
import Follow
import Follow.Middlewares.Sort

-- Suppose we have a `Directory` d

apply (byGetter eTitle) d
@

-}
module Follow.Middlewares.Sort
  ( apply
  , byGetter
  , ComparisonFunction
  ) where

import           Data.List    (sortBy)
import           Follow.Types (Directory (..), Entry, EntryGetter, Middleware)

-- | Function to compare entries.
type ComparisonFunction = Entry -> Entry -> Ordering

-- | Middleware to sort a directory according to a given comparison function.
apply :: ComparisonFunction -> Middleware
apply f directory = directory {dEntries = sortBy f (dEntries directory)}

-- | Creates a comparison function that sorts by the values returned
-- by a getter
byGetter :: Ord a => EntryGetter a -> ComparisonFunction
byGetter getter e1 e2 = compare (getter e1) (getter e2)
