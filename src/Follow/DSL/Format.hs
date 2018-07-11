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
  , nameFormat
  , versionLineFormat
  , titleLineFormat
  , descriptionLineFormat
  , tagsLineFormat
  , versionFormat
  , titleFormat
  , descriptionFormat
  , tagsFormat
  , wordFormat
  , multiWordFormat
  , cswFormat
  , innerLineFormat
  , endingLineFormat
  ) where

import           Data.Char    (isPunctuation, isSymbol)
import           Data.Functor (($>))
import           Text.Parsec

-- | Format expected for the DSL String
format :: Parsec String () (String, String, String, [String])
format = do
  version <- versionLineFormat
  title <- titleLineFormat
  description <- descriptionLineFormat
  tags <- tagsLineFormat
  return (version, title, description, tags)

-- | Format for a line which is not the ending line
innerLineFormat :: String -> Parsec String () a -> Parsec String () a
innerLineFormat = lineFormat False

-- | Format for the ending line
endingLineFormat :: String -> Parsec String () a -> Parsec String () a
endingLineFormat = lineFormat True

-- | Format for the line containg the DSL version
versionLineFormat :: Parsec String () String
versionLineFormat = innerLineFormat "VERSION" versionFormat

-- | Format for the version number used in the version line
versionFormat :: Parsec String () String
versionFormat = do
  major <- many1 digit
  char '.'
  minor <- many1 digit
  return $ major ++ "." ++ minor

-- | Format for the line containing the recipe title
titleLineFormat :: Parsec String () String
titleLineFormat = innerLineFormat "TITLE" titleFormat

-- | Format for the recipe title
titleFormat :: Parsec String () String
titleFormat = multiWordFormat

-- | Format for the line containing the recipe description
descriptionLineFormat :: Parsec String () String
descriptionLineFormat = innerLineFormat "DESCRIPTION" descriptionFormat

-- | Format for the recipe description
descriptionFormat :: Parsec String () String
descriptionFormat = multiWordFormat

-- | Format for the line containing the recipe tags
tagsLineFormat :: Parsec String () [String]
tagsLineFormat = endingLineFormat "TAGS" tagsFormat

-- | Format for the recipe tags
tagsFormat :: Parsec String () [String]
tagsFormat = cswFormat

-- | Format for the expected reserved words in the DSL
nameFormat :: String -> Parsec String () String
nameFormat = string

-- | Format for a value consisting of multiple words
multiWordFormat :: Parsec String () String
multiWordFormat = unwords <$> sepEndBy1 wordFormat spaceFormat

-- | Format for a value consisting of a comma separated list of words
cswFormat :: Parsec String () [String]
cswFormat =
  sepEndBy1
    (unwords <$> sepEndBy1 (many1 alphaNum) spaceFormat)
    (optional spaceFormat *> char ',' *> optional spaceFormat)

-- | Format for what is considered a word
wordFormat :: Parsec String () String
wordFormat = many1 (alphaNum <|> satisfy isPunctuation <|> satisfy isSymbol)

lineFormat :: Bool -> String -> Parsec String () a -> Parsec String () a
lineFormat isEnding name valueFormat =
  optionalSpaceFormat *> nameFormat name *> spaceFormat *> valueFormat <*
  (if isEnding
     then endingLineEndFormat
     else innerLineEndFormat)

spaceFormat :: Parsec String () ()
spaceFormat = many1 (oneOf " \t") $> ()

optionalSpaceFormat :: Parsec String () ()
optionalSpaceFormat = optional spaceFormat

innerLineEndFormat :: Parsec String () ()
innerLineEndFormat = optionalSpaceFormat *> endOfLine $> ()

endingLineEndFormat :: Parsec String () ()
endingLineEndFormat = optionalSpaceFormat *> optional endOfLine *> eof $> ()
