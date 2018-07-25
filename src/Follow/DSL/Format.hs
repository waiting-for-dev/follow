{- |
Description: Defines formats to be used when composing formats for
             fetcher argument values.
-}
module Follow.DSL.Format
  ( Parse
  , wordFormat
  , multiWordFormat
  , csFormat
  , spaceFormat
  , optionalSpaceFormat
  , uriFormat
  ) where

import           Control.Monad (mfilter)
import           Data.Char     (isPunctuation, isSymbol)
import           Data.Functor  (($>))
import           Follow.Types  (Parse)
import           Network.URI   (isAbsoluteURI, isAllowedInURI)
import           Text.Parsec

-- | Format for what is considered a word. It allows any combination
-- of letters, digits, punctuations and symbols.
wordFormat :: Parse String
wordFormat = many1 (alphaNum <|> satisfy isPunctuation <|> satisfy isSymbol)

-- Format for multiple words together (like a sentence). It is one or
-- more `wordFormat` separated by `spaceFormat`.
multiWordFormat :: Parse String
multiWordFormat = unwords <$> sepEndBy1 wordFormat spaceFormat

-- Format for a comma separated lists of items of given format.
csFormat :: Parse a -> Parse [a]
csFormat itemFormat =
  sepEndBy1 itemFormat (optionalSpaceFormat *> char ',' *> optionalSpaceFormat)

-- Format for a URI
uriFormat :: Parse String
uriFormat = mfilter isAbsoluteURI (many1 $ satisfy isAllowedInURI)

-- Horizontal space format: one or more spaces or tabs.
spaceFormat :: Parse ()
spaceFormat = many1 (oneOf " \t") $> ()

-- Format meaning no space or `spaceFormat`.
optionalSpaceFormat :: Parse ()
optionalSpaceFormat = optional spaceFormat
