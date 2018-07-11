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

import           Follow.DSL.Format (format)
import           Text.Parsec

-- | Haskell representation of the information extracted from the
-- DSL. It contains the information needed to fetch the content for a
-- followee.
data Recipe = Recipe
  { rVersion     :: String -- ^ Version of the DSL used.
  , rTitle       :: String -- ^ Title for the recipe; what is being followed.
  , rDescription :: String -- ^ A description for the recipe
  , rTags        :: [String] -- ^ Tags that apply to the recipe
  } deriving (Show)

-- | The result of a parsing: a `Recipe` or an error.
type ParseResult = Either ParseError Recipe

-- | Parses DSL from a string to either a `Recipe` or an error.
parseDSL :: String -> ParseResult
parseDSL toParse =
  case parse format "(source)" toParse of
    Left error -> Left error
    Right (version, title, description, tags) ->
      Right (Recipe version title description tags)
