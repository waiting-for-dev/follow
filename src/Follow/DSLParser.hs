{- |
Description: Defines and parses the DSL for followees recipes.

The DSL for a recipe must conform with following format:

@
VERSION 1.0
FOLLOW Joe Doe
DESCRIBED BY All the amazing articles from Joe Doe
@

That recipe could be parsed with 'parseDSL', which would return a
'ParsedDSL' data type with the extracted information.
-}
module Follow.DSLParser
  (
    parseDSL
  , ParsedDSL (..)
  , ParsedResult
  ) where

import           Text.Parsec

type PVersion = String
type PTitle = String
type PDescription = String

-- | Haskell representation of the information extracted from the DSL.
data ParsedDSL = ParsedDSL
  {  pVersion     :: PVersion -- ^ Version of the DSL used.
  ,  pTitle       :: PTitle -- ^ Title for the recipe; what is being followed.
  ,  pDescription :: PDescription -- ^ A description for the recipe
  } deriving (Show)

-- | The result of a parsing: a 'ParsedDSL' or an error.
type ParsedResult = Either ParseError ParsedDSL

format :: Parsec String () (PVersion, PTitle, PDescription)
format = do
  version <- versionFormat
  title <- titleFormat
  description <- descriptionFormat
  return (version, title, description)

versionFormat :: Parsec String () PVersion
versionFormat = spaces *> string "VERSION" *> spaces *> many1 (noneOf "\n\r") <* endOfLine

titleFormat :: Parsec String () PTitle
titleFormat = spaces *> string "FOLLOW" *> spaces *> many1 (noneOf "\n\r") <* endOfLine

descriptionFormat :: Parsec String () PDescription
descriptionFormat = spaces *> string "DESCRIBED BY" *> spaces *> many1 (noneOf "\n\r") <* optional endOfLine

-- | Parses given DSL to a 'ParsedDSL' on success.
parseDSL :: String -> ParsedResult
parseDSL toParse = case parse format "(source)" toParse of
  Left error -> Left error
  Right (version, title, description) -> Right (ParsedDSL version title description)
