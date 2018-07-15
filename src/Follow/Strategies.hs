{- |
Description: Top namespace for the different strategies to fetch the
             information for a recipe.

This module defines the common data types to be consumed by any
concrete strategy, along with any other general function.
-}
module Follow.Strategies
  ( validStrategies
  , Arguments
  , ArgumentsDSL
  , Value(..)
  ) where

import           Text.Parsec (Parsec)

type Name = String

-- | Union type with the valid types for an argument value.
data Value
  = VString String
  | VStringList [String]
  deriving (Show, Eq)

-- | List of arguments; pairs of name and value.
type Arguments = [(Name, Value)]

-- | Concrete strategies must define an ArgumentsDSL method that
-- defines a mapping between argument names and its expected DSL for the
-- value
type ArgumentsDSL = [(Name, Parsec String () Value)]

-- | Strateges that are known by the system
validStrategies :: [String]
validStrategies = ["null"]
