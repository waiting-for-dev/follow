module Follow.Fetchers.Feed
  ( argumentsDSL
  ) where

import           Data.Dynamic      (toDyn)

import           Follow.DSL.Format (uriFormat)
import           Follow.Types      (ArgumentsDSL)

argumentsDSL :: ArgumentsDSL
argumentsDSL = [("URL", toDyn <$> uriFormat)]
