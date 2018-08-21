{-|
Description: Middleware to filter entries according a predicate.

This middleware allows to filter the directory entries according to a
predicate. The predicate is a function @Entry -> Bool@ (defined in
`Follow.Types.MFilterPredicate`).

There are some pre-built predicate builders. Exemple:

@
import Follow.Types (Entry(..), MiddlewareArguments (..))
import Follow.Middlewares.Filter

-- Suppose we have a `directory`

apply (MFilterPredicate (eTitle `equal` "Some title")) directory
@

-}
module Follow.Middlewares.Filter where

import           Follow.Middlewares.Filter.Internal
import           Follow.Types                       (Directory (..), Middleware,
                                                     MiddlewareArguments (..))

-- | See `Follow.Types.Middleware`.
apply :: Middleware
apply (MFilterPredicate p) directory =
  directory {dEntries = filter p (dEntries directory)}
