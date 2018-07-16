{- |
Description: Top namespace for the different strategies to fetch the
             information for a recipe.

This module defines the common data types to be consumed by any
concrete strategy, along with any other general function.
-}
module Follow.Strategies
  ( Arguments
  , ArgumentsDSL
  ) where

import           Data.Dynamic (Dynamic)
import           Text.Parsec  (Parsec)

type Name = String

-- | List of arguments; pairs of name and value.
type Arguments = [(Name, Dynamic)]

-- | Concrete strategies must define an ArgumentsDSL method that
-- defines a mapping between argument names and its expected DSL for the
-- value
type ArgumentsDSL = [(Name, Parsec String () Dynamic)]
