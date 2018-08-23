{- |
Description: Parses the DSL defined in a recipe to a haskell datatype.

As defined in "Follow", `Follow.Recipe` is the haskell representation
of the information contained in a recipe DSL. When a parsing is done,
a `ParseResult` is obtained, which can be either the recipe or a
parsing error.

For details on the format of the DSL, check "Follow.DSL.Format" module.
-}
module Follow.DSL.Parser
  ( parseDSL
  , parseDSLFile
  ) where

import           Data.Text                  (pack)
import           Follow.DSL.Format.Internal (format)
import           Follow.Types               (ArgumentsDSL, Header (..),
                                             ParseResult)
import           Text.Parsec                (ParseError, parse)

-- | Parses DSL from a string.
parseDSL :: String -> ArgumentsDSL -> ParseResult
parseDSL = parse' "(source string)"

-- | Parses DSL from a file.
parseDSLFile :: FilePath -> ArgumentsDSL -> IO ParseResult
parseDSLFile path argumentsDSL =
  readFile path >>= \contents -> return $ parse' path contents argumentsDSL

parse' :: String -> String -> ArgumentsDSL -> ParseResult
parse' source toParse argumentsDSL =
  case parse (format argumentsDSL) source toParse of
    Left error -> Left error
    Right (title, description, tags) ->
      Right (Header (pack title) (pack description) (pack <$> tags))
