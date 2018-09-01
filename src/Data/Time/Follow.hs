{- |
Description: Date and time utilities used in the library.

This module exposes functions related with date and time which are
used in other parts of the library.
-}
module Data.Time.Follow
  ( parseTimeGuess
  ) where

import           Control.Applicative ((<|>))
import           Data.Text           (Text)
import qualified Data.Text           as T (unpack)
import           Data.Time           (UTCTime, defaultTimeLocale, formatTime,
                                      iso8601DateFormat, parseTimeM,
                                      rfc822DateFormat)

-- | Parses a date guessing from a list of accepted formats. Right now
-- these are:
--   - iso8601: 2018-01-01
--   - iso8601 with time: 2018-01-01T12:35
--   - iso8601 with time and timezone: 2018-01-01T12:35+0100
--   - iso8601 with fractions of seconds: 2018-01-01T12:35.001000
--   - iso8601 with fractions of seconds and timezone:
--   2018-01-01T12:35.001000+0100
--   - rfc822: Mon, 01 Jan 2018 12:35:00 +0100
--   - epoch: 1514810100
parseTimeGuess :: Text -> Maybe UTCTime
parseTimeGuess string =
  let formats =
        [ iso8601DateFormat Nothing
        , iso8601DateFormat $ Just "%H:%M:%S%Z"
        , iso8601DateFormat $ Just "%H:%M:%S%Q%Z"
        , rfc822DateFormat
        , "%s"
        ]
      results = parseTime' (T.unpack string) <$> formats
   in foldl1 (<|>) results
  where
    parseTime' = flip $ parseTimeM True defaultTimeLocale
