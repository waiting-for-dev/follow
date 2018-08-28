{-|
Description: Middleware to filter entries according to a predicate.

This middleware allows to filter the directory entries according to a
predicate. The predicate is a function @Entry -> Bool@.

Some pre-built predicate builders are also exported. Example:

@
import Follow
import Follow.Middlewares.Filter

-- Suppose we have a `Directory` d

apply (eTitle `equalP` "Some title") d
@

-}
module Follow.Middlewares.Filter
  ( apply
  , Predicate
  , equalP
  , infixP
  , prefixP
  , suffixP
  , andP
  , orP
  , notP
  ) where

import           Follow.Middlewares.Filter.Internal
import           Follow.Types                       (Directory (..), Middleware)

-- | Middleware to filter a directory according to a given predicate.
apply :: Predicate -> Middleware
apply p directory = directory {dEntries = filter p (dEntries directory)}
