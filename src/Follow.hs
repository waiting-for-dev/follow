{- |
Description: Defines and reexports application level components.

Read the [README](https://github.com/waiting-for-dev/follow#readme)
file for a description of `Follow` application.

Importing this module makes all application scope components
available. In addition, you probably will need to import individual
fetchers, middlewares and digesters.
-}
module Follow
  ( Recipe(..)
  , Subject(..)
  , Entry(..)
  , Fetched
  , Directory(..)
  , Step
  , Middleware
  , Digester
  , directoryFromRecipe
  , directoryFromFetched
  , applyMiddlewares
  , applySteps
  , emptyDirectory
  , mergeEntries
  ) where

import           Control.Monad.Catch (MonadThrow)
import           Data.Foldable       (foldlM)
import           Data.List           (nub)
import           Follow.Types        (Digester, Directory (..), Entry (..),
                                      Fetched, Middleware, Recipe (..), Step,
                                      Subject (..))

-- | Builds a directory from the specification stored in a recipe
directoryFromRecipe :: MonadThrow m => Recipe m -> m Directory
directoryFromRecipe recipe =
  let Recipe {rSubject = subject, rSteps = steps, rMiddlewares = middlewares} =
        recipe
   in applyMiddlewares middlewares <$> applySteps (emptyDirectory subject) steps

-- | Helper to build a directory from a subject and a list of fetched
-- entries.
directoryFromFetched :: MonadThrow m => m [Entry] -> Subject -> m Directory
directoryFromFetched fetched header = Directory header <$> fetched

-- | Applies, from left to right, given middlewares to the directory.
applyMiddlewares :: [Middleware] -> Directory -> Directory
applyMiddlewares = flip $ foldl (flip ($))

-- | Applies, from left to right, a list of steps to given directory.
applySteps :: MonadThrow m => Directory -> [Step m] -> m Directory
applySteps = foldlM applyStep
  where
    applyStep directory (fetched, middlewares) =
      applyMiddlewares middlewares . mergeEntries directory <$> fetched

-- | Creates a directory with given subject and no entries.
emptyDirectory :: Subject -> Directory
emptyDirectory s = Directory {dSubject = s, dEntries = mempty}

-- | Merges a list of entries into a Directory, keeping only first
-- appearance of duplicates.
mergeEntries :: Directory -> [Entry] -> Directory
mergeEntries d e = d {dEntries = nub $ dEntries d ++ e}
