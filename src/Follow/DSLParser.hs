{- |
Description: Defines and parses the DSL for followees recipes.

The DSL for a recipe must conform with following format:

@
VERSION 1.0
TITLE Joe Doe
DESCRIPTION All the amazing articles from Joe Doe
@

That recipe could be parsed with 'parseDSL', which would return a
'Recipe' data type with the extracted information.
-}
module Follow.DSLParser
  ( parseDSL
  , Recipe(..)
  , ParseResult
  ) where

import           Text.Parsec

type RVersion = String

type RTitle = String

type RDescription = String

-- | Recipe to fetch content for a followee; haskell representation of
-- the information extracted from the DSL.
data Recipe = Recipe
  { rVersion     :: RVersion -- ^ Version of the DSL used.
  , rTitle       :: RTitle -- ^ Title for the recipe; what is being followed.
  , rDescription :: RDescription -- ^ A description for the recipe
  } deriving (Show)

-- | The result of a parsing: a 'Recipe' or an error.
type ParseResult = Either ParseError Recipe

format :: Parsec String () (RVersion, RTitle, RDescription)
format = do
  version <- versionFormat
  title <- titleFormat
  description <- descriptionFormat
  return (version, title, description)

versionFormat :: Parsec String () RVersion
versionFormat =
  spaces *> string "VERSION" *> spaces *> many1 (noneOf "\n\r") <* endOfLine

titleFormat :: Parsec String () RTitle
titleFormat =
  spaces *> string "TITLE" *> spaces *> many1 (noneOf "\n\r") <* endOfLine

descriptionFormat :: Parsec String () RDescription
descriptionFormat =
  spaces *> string "DESCRIPTION" *> spaces *> many1 (noneOf "\n\r") <*
  optional endOfLine

-- | Parses given DSL to a 'Recipe' on success.
parseDSL :: String -> ParseResult
parseDSL toParse =
  case parse format "(source)" toParse of
    Left error -> Left error
    Right (version, title, description) ->
      Right (Recipe version title description)
