{- |
Description: Defines and reexports application level components.

Read the [README](https://github.com/waiting-for-dev/follow#readme)
file for a description of `Follow` application.

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
  , processRecipe
  ) where

import           Data.Foldable      (foldlM)
import           Follow.Digesters   (digest)
import           Follow.Fetchers    (buildDirectory)
import           Follow.Middlewares (applyMiddlewares)
import           Follow.Types       (Digester, Directory (..), Entry (..),
                                     Fetched, Fetcher, Middleware, Recipe (..),
                                     Result, Subject (..))

-- | Fetches, apply middlewares and digests using given subject
process :: Fetched -> [Middleware] -> Digester a -> Subject -> Result a
process fetched middlewares digester subject =
  digest digester . applyMiddlewares middlewares <$>
  buildDirectory fetched subject

-- | Builds a directory from the specification stored in a recipe
processRecipe :: Recipe -> Result Directory
processRecipe recipe =
  let Recipe {rSubject = subject, rSteps = steps, rMiddlewares = middlewares} =
        recipe
   in applyMiddlewares middlewares <$> applySteps (initDir subject) steps

applySteps :: Directory -> [(Fetched, [Middleware])] -> Result Directory
applySteps = foldlM applyStep
  where
    applyStep :: Directory -> (Fetched, [Middleware]) -> Result Directory
    applyStep directory (fetched, middlewares) =
      applyMiddlewares middlewares . concatEntries directory <$> fetched

initDir :: Subject -> Directory
initDir s = Directory {dSubject = s, dEntries = mempty}

concatEntries :: Directory -> [Entry] -> Directory
concatEntries d e = d {dEntries = dEntries d ++ e}
