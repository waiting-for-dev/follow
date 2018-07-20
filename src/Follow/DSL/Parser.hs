{- |
Description: Parses the DSL defined in a recipe to a haskell datatype.

As defined in "Follow", `Follow.Recipe` is the haskell representation
of the information contained in a recipe DSL. When a parsing is done,
a `ParseResult` is obtained, which can be either the recipe or a
parsing error.

For details on the format of the DSL, check "Follow.DSL.Format" module.
-}
module Follow.DSL.Parser
  ( parseDSL
  , ParseResult
  ) where

import           Follow                     (Recipe (..))
import           Follow.DSL.Format.Internal (format)
import           Follow.Strategies          (ArgumentsDSL)
import           Text.Parsec                (ParseError, parse)

-- | The result of a parsing: a `Follow.Recipe` or an error.
type ParseResult = Either ParseError Recipe

-- | Parses DSL from a string to either a `Recipe` or an error.
parseDSL :: String -> ArgumentsDSL -> ParseResult
parseDSL toParse argumentsDSL =
  case parse (format argumentsDSL) "(source)" toParse of
    Left error -> Left error
    Right (version, title, description, tags, strategyArguments) ->
      Right (Recipe version title description tags strategyArguments)
