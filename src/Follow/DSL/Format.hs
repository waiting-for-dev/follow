{- |
Description: Defines the format for the DSL used in followees recipes.

The DSL for a recipe must conform with following format:

@
VERSION 1.0
TITLE Joe Doe
DESCRIPTION All the amazing articles from Joe Doe
@
-}
module Follow.DSL.Format
  ( format
  , lineFormat
  , nameFormat
  , versionLineFormat
  , versionFormat
  , titleLineFormat
  , titleFormat
  , descriptionLineFormat
  , descriptionFormat
  , tagsLineFormat
  , tagsFormat
  , tagFormat
  , argumentsFormat
  , wordFormat
  , multiWordFormat
  , csFormat
  ) where

import           Data.Char         (isPunctuation, isSymbol)
import           Data.Functor      (($>))
import           Follow.Strategies (Arguments, ArgumentsDSL, Value (..))
import           Text.Parsec

type Parse = Parsec String ()

-- | Format expected for the DSL String
format :: ArgumentsDSL -> Parse (String, String, String, [String], Arguments)
format argumentsDSL = do
  version <- versionLineFormat
  endOfLine
  title <- titleLineFormat
  endOfLine
  description <- descriptionLineFormat
  endOfLine
  tags <- tagsLineFormat
  -- next endOfLine is parsed in argumentsFormat
  arguments <- argumentsFormat argumentsDSL
  optional endOfLine
  return (version, title, description, tags, arguments)

-- | Format for a line consisting of a name/value pair
lineFormat :: String -> Parse a -> Parse a
lineFormat name valueFormat =
  optionalSpaceFormat *> nameFormat name *> spaceFormat *> valueFormat <*
  optionalSpaceFormat

-- | Format for the line containg the DSL version
versionLineFormat :: Parse String
versionLineFormat = lineFormat "VERSION" versionFormat

-- | Format for the version number used in the version line
versionFormat :: Parse String
versionFormat = do
  major <- many1 digit
  char '.'
  minor <- many1 digit
  return $ major ++ "." ++ minor

-- | Format for the line containing the recipe title
titleLineFormat :: Parse String
titleLineFormat = lineFormat "TITLE" titleFormat

-- | Format for the recipe title
titleFormat :: Parse String
titleFormat = multiWordFormat

-- | Format for the line containing the recipe description
descriptionLineFormat :: Parse String
descriptionLineFormat = lineFormat "DESCRIPTION" descriptionFormat

-- | Format for the recipe description
descriptionFormat :: Parse String
descriptionFormat = multiWordFormat

-- | Format for the line containing the recipe tags
tagsLineFormat :: Parse [String]
tagsLineFormat = lineFormat "TAGS" tagsFormat

-- | Format for the recipe tags
tagsFormat :: Parse [String]
tagsFormat = csFormat tagFormat

-- | Format for a tag
tagFormat :: Parse String
tagFormat = many1 (alphaNum <|> oneOf "_-")

-- | Format for the arguments to expect for given dsl.
argumentsFormat :: ArgumentsDSL -> Parse Arguments
argumentsFormat dsl = sequence (toSteps dsl)
  where
    toSteps =
      foldl
        (\acc (name, format) ->
           ((,) name <$> (endOfLine *> lineFormat name format)) : acc)
        []

-- | Format for the expected reserved words in the DSL
nameFormat :: String -> Parse String
nameFormat = string

-- | Format for a value consisting of multiple words
multiWordFormat :: Parse String
multiWordFormat = unwords <$> sepEndBy1 wordFormat spaceFormat

-- | Format for a value consisting of a comma separated list of words
csFormat :: Parse String -> Parse [String]
csFormat itemFormat =
  sepEndBy1
    (unwords <$> sepEndBy1 itemFormat spaceFormat)
    (optional spaceFormat *> char ',' *> optional spaceFormat)

-- | Format for what is considered a word
wordFormat :: Parse String
wordFormat = many1 (alphaNum <|> satisfy isPunctuation <|> satisfy isSymbol)

spaceFormat :: Parse ()
spaceFormat = many1 (oneOf " \t") $> ()

optionalSpaceFormat :: Parse ()
optionalSpaceFormat = optional spaceFormat
