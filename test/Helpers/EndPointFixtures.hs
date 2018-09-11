{-# LANGUAGE OverloadedStrings #-}

module Helpers.EndPointFixtures
  ( simpleEndPoint
  , endPointWithStatus
  , feedEndPoint
  , webScrapingEndPoint
  , invalidEndPoint
  ) where

import qualified Data.ByteString       as BS (ByteString, concat)
import qualified Data.ByteString.Char8 as BS (pack)

simpleEndPoint :: BS.ByteString
simpleEndPoint = "http://httpbin.org"

endPointWithStatus :: Int -> BS.ByteString
endPointWithStatus status =
  BS.concat ["http://httpbin.org/status/", BS.pack $ show status]

feedEndPoint :: BS.ByteString
feedEndPoint = "http://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"

webScrapingEndPoint :: BS.ByteString
webScrapingEndPoint = "https://www.nytimes.com/section/world"

invalidEndPoint :: BS.ByteString
invalidEndPoint = "invalidurl"
