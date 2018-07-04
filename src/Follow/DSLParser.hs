module Follow.DSLParser
  (
    parseDSL
  , ParsedDSL (..)
  ) where

import Text.Parsec

data ParsedDSL = ParsedDSL
  { parsedDSLTitle :: String
  }

type ParsedResult = Either ParseError ParsedDSL

format :: Parsec String () String
format = spaces *> string "FOLLOW" *> spaces *> many1 anyChar

parseDSL :: String -> ParsedResult
parseDSL toParse = case parse format "(source)" toParse of
  Left error -> Left error
  Right title -> Right (ParsedDSL { parsedDSLTitle = title })
