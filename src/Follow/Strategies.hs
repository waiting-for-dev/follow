{- |
Description: Top namespace to define strategies to fetch recipes.

This module defines the common data types to be consumed by any
concrete strategy, along with any other general function.

The DSL for a recipe must contain some common fields (see "Follow.DSL"
for details). From there, an strategy can define new ones through an
`ArgumentsDSL` data type. This data type is just a list of tuples
/name-value/, where /name/ is the argument name, and /value/ is a
format that the value is expected to obey (different formats ready to
be used are defined in "Follow.DSL.Format", and reexported in
"Follow.DSL").

As the value could be anything, its type will be dynamic (see
"Data.Dynamic").

Here it is an example. Say we have following definition:

@
import Follow.DSL
import Follow.Strategies
import Data.Dynamic

argumentsDSL :: ArgumentsDSL
argumentsDSL = [("ARG1", toDyn <$> wordFormat), ("ARG2", toDyn <$> csFormat wordFormat)]
@

It would be useful to parse a recipe with following lines (base DSL not included):

@
ARG1 value1
ARG2 val1, val2
@
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
