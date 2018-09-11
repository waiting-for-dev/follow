{-# LANGUAGE OverloadedStrings #-}

module Follow.Fetchers.FeedSpec where

import           Data.Maybe               (fromJust)
import qualified Data.Text                as T (isInfixOf, pack)
import           Follow.Fetchers.Feed
import           Follow.Types             (Entry (..))
import           Helpers.EndPointFixtures (endPointWithStatus, feedEndPoint,
                                           invalidEndPoint, simpleEndPoint)
import           HTTP.Follow              (HTTPError (..))
import qualified Network.HTTP.Req         as R (HttpException (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetcher" $ do
    it "fetches entries from given url" $ do
      entries <- fetch feedEndPoint
      let entry = head entries
      let url = (fromJust . eURI) entry
      T.isInfixOf "nytimes" url `shouldBe` True
    it "returns error when URL is not valid" $ do
      fetch invalidEndPoint `shouldThrow` (== URLWrongFormat)
    it "returns error when response can't be parsed to a feed" $ do
      fetch simpleEndPoint `shouldThrow` (== FeedWrongFormat)
    it "returns the http error when it happens" $ do
      fetch (endPointWithStatus 404) `shouldThrow` \e ->
        "HttpException" `T.isInfixOf` (T.pack $ show (e :: R.HttpException))
