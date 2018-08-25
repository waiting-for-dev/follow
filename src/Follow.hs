{- |
Description: Defines and reexports application level components.

Read the README file for a description of `Follow` application.

Importing this module makes all application scope components
available. In addition, you probably will need to import individual
fetchers, middlewares and digesters.
-}
module Follow
  ( module Follow.Fetchers
  , module Follow.Digesters
  , module Follow.Middlewares
  , module Follow.Types
  , process
  ) where

import           Follow.Digesters   (digest)
import           Follow.Fetchers    (buildDirectory)
import           Follow.Middlewares (applyMiddlewares)
import           Follow.Types       (Digester, Directory (..), Entry (..),
                                     Fetched, Fetcher, Middleware, Result,
                                     Subject (..))

-- | Fetches, apply middlewares and digests using given subject
process :: Fetched -> [Middleware] -> Digester a -> Subject -> Result a
process fetched middlewares digester subject =
  digest digester . applyMiddlewares middlewares <$>
  buildDirectory fetched subject
