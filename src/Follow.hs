{- | Description: Top namespace for follow application.

Reexports main funtions to be performed at the application scope and
defines global operational functions.
-}
module Follow
  ( module Follow.Fetchers
  , module Follow.Digesters
  , module Follow.Middlewares
  , process
  ) where

import           Follow.Digesters   (digest)
import           Follow.Fetchers    (fetch)
import           Follow.Middlewares (applyMiddlewares)
import           Follow.Types       (Digester, Fetcher, Middleware,
                                     MiddlewareArguments, Recipe, Result)

-- | Fetches and digests a recipe using given strategies
process ::
     Fetcher
  -> [(Middleware, MiddlewareArguments)]
  -> Digester a
  -> Recipe
  -> Result a
process fetcher middlewares digester recipe =
  digest digester . applyMiddlewares middlewares <$> fetch fetcher recipe
