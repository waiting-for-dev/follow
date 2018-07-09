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

import           Data.Functor (($>))
import           Text.Parsec

-- | Format expected for the DSL String
format :: Parsec String () (String, String, String)
format = do
  version <- versionFormat
  title <- titleFormat
  description <- descriptionFormat
  return (version, title, description)

innerLineFormat :: String -> Parsec String () String -> Parsec String () String
innerLineFormat name valueFormat =
  lineStart *> string name *> lineSeparation *> valueFormat <* innerLineEnd

endingLineFormat :: String -> Parsec String () String -> Parsec String () String
endingLineFormat name valueFormat =
  lineStart *> string name *> lineSeparation *> valueFormat <* endingLineEnd

lineStart :: Parsec String () ()
lineStart = optional lineSeparation

lineSeparation :: Parsec String () ()
lineSeparation = many1 (oneOf " \t") $> ()

innerLineEnd :: Parsec String () ()
innerLineEnd = optional lineSeparation *> endOfLine $> ()

endingLineEnd :: Parsec String () ()
endingLineEnd = optional lineSeparation *> optional endOfLine $> ()

versionFormat :: Parsec String () String
versionFormat = innerLineFormat "VERSION" $ many1 $ digit <|> char '.'

titleFormat :: Parsec String () String
titleFormat = innerLineFormat "TITLE" $ many1 alphaNum

descriptionFormat :: Parsec String () String
descriptionFormat =
  spaces *> string "DESCRIPTION" *> spaces *> many1 alphaNum <*
  optional endOfLine
