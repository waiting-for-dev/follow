module Follow.Middlewares.Filter where

import           Follow.Middlewares.Filter.Internal
import           Follow.Types                       (Directory (..), Middleware,
                                                     MiddlewareArguments (..))

apply :: Middleware
apply (MFilterPredicate p) directory =
  directory {dEntries = filter p (dEntries directory)}
