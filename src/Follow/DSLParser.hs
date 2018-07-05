module Follow.DSLParser
  (
    parseDSL
  , ParsedDSL (..)
  ) where

import Text.Parsec

data ParsedDSL = ParsedDSL
  {  pVersion :: String
  ,  pTitle :: String
  ,  pDescription :: String
  } deriving (Show)

type ParsedResult = Either ParseError ParsedDSL

format :: Parsec String () (String, String, String)
format = do
  version <- versionFormat
  title <- titleFormat
  description <- descriptionFormat
  return (version, title, description)

versionFormat :: Parsec String () String
versionFormat = spaces *> string "VERSION" *> spaces *> many1 (noneOf "\n\r") <* endOfLine

titleFormat :: Parsec String () String
titleFormat = spaces *> string "FOLLOW" *> spaces *> many1 (noneOf "\n\r") <* endOfLine

descriptionFormat :: Parsec String () String
descriptionFormat = spaces *> string "DESCRIBED BY" *> spaces *> many1 (noneOf "\n\r") <* optional endOfLine

parseDSL :: String -> ParsedResult
parseDSL toParse = case parse format "(source)" toParse of
  Left error -> Left error
  Right (version, title, description) -> Right (ParsedDSL {
                                                     pVersion = version
                                                   , pTitle = title
                                                   , pDescription = description })
