{- |
Description: Parses the DSL defined in a recipe to a haskell datatype.

`Recipe` is the haskell representation of the information contained in
a recipe DSL. When a parsing is done, a `ParseResult` is obtained, which
can be either the `Recipe` or a parsing error.

For details on the format of the DSL, check "Follow.DSL.Format" module.
-}
module Follow.DSL.Parser
  ( parseDSL
  , Recipe(..)
  , ParseResult
  ) where

import           Follow.DSL.Format.Internal (Description, Tags, Title, Version,
                                             format)
import           Follow.Strategies          (Arguments, ArgumentsDSL)
import           Text.Parsec                (ParseError, parse)

-- | Haskell representation of the information extracted from the
-- DSL. It contains the information needed to fetch the content for a
-- followee.
data Recipe = Recipe
  { rVersion           :: Version -- ^ Version of the DSL used.
  , rTitle             :: Title -- ^ Title for the recipe; what is being followed.
  , rDescription       :: Description -- ^ A description for the recipe
  , rTags              :: Tags -- ^ Tags that apply to the recipe
  , rStrategyArguments :: Arguments -- ^ Arguments to be given to the strategy
  } deriving (Show)

-- | The result of a parsing: a `Recipe` or an error.
type ParseResult = Either ParseError Recipe

-- | Parses DSL from a string to either a `Recipe` or an error.
parseDSL :: String -> ArgumentsDSL -> ParseResult
parseDSL toParse argumentsDSL =
  case parse (format argumentsDSL) "(source)" toParse of
    Left error -> Left error
    Right (version, title, description, tags, strategyArguments) ->
      Right (Recipe version title description tags strategyArguments)
