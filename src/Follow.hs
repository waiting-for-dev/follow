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
import           Follow.Types       (Digester, Fetched, Header, Middleware,
                                     Result)

-- | Fetches, apply middlewares and digests using given header
process :: Fetched -> [Middleware] -> Digester a -> Header -> Result a
process fetched middlewares digester header =
  digest digester . applyMiddlewares middlewares <$> fetch fetched header
