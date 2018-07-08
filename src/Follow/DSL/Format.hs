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
  ) where

import           Text.Parsec

-- | Format expected for the DSL String
format :: Parsec String () (String, String, String)
format = do
  version <- versionFormat
  title <- titleFormat
  description <- descriptionFormat
  return (version, title, description)

versionFormat :: Parsec String () String
versionFormat =
  spaces *> string "VERSION" *> spaces *> many1 (noneOf "\n\r") <* endOfLine

titleFormat :: Parsec String () String
titleFormat =
  spaces *> string "TITLE" *> spaces *> many1 (noneOf "\n\r") <* endOfLine

descriptionFormat :: Parsec String () String
descriptionFormat =
  spaces *> string "DESCRIPTION" *> spaces *> many1 (noneOf "\n\r") <*
  optional endOfLine
