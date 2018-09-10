{-# LANGUAGE OverloadedStrings #-}

module Follow.Fetchers.WebScrapingSpec where

import           Data.Maybe                  (fromJust)
import qualified Data.Text                   as T (isInfixOf, pack)
import           Follow.Fetchers.WebScraping
import           Follow.Types                (Entry (..))
import           Helpers.EndPointFixtures
import           Helpers.Factories
import           HTTP.Follow                 (HTTPError (..))
import qualified Network.HTTP.Req            as R (HttpException (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".fetch" $ do
    let selector = _selector {selURI = Just (Attr ".headline a" "href")}
    it "fetches entries from given url" $ do
      entries <- fetch webScrapingEndPoint selector
      let entry = head entries
      let url = (fromJust . eURI) entry
      T.isInfixOf "nytimes" url `shouldBe` True
    it "returns error when URL is not valid" $ do
      fetch invalidEndPoint selector `shouldThrow` (== URLWrongFormat)
    it "returns the http error when it happens" $ do
      fetch (endPointWithStatus 404) selector `shouldThrow` \e ->
        "HttpException" `T.isInfixOf` (T.pack $ show (e :: R.HttpException))
