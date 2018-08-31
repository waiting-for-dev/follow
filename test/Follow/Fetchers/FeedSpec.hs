{-# LANGUAGE OverloadedStrings #-}

module Follow.Fetchers.FeedSpec where

import qualified Data.ByteString          as BS (ByteString)
import qualified Data.ByteString.Char8    as BS (unpack)
import           Data.Dynamic             (toDyn)
import           Data.Either              (isRight)
import           Data.Maybe               (fromJust)
import           Data.Text                (isInfixOf)
import           Follow.Fetchers.Feed
import           Follow.Types             (Entry (..), FetchError (..),
                                           FetchFeedError (..), Result,
                                           unwrapResult)
import           Helpers.EndPointFixtures (endPointWithStatus, feedEndPoint,
                                           invalidEndPoint, simpleEndPoint)
import qualified Network.HTTP.Client      as H (HttpException (..),
                                                HttpExceptionContent (..),
                                                responseStatus)
import qualified Network.HTTP.Req         as R (HttpException (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetcher" $ do
    it "fetches entries from given url" $ do
      let entries = fetch feedEndPoint
      let entry = fmap head entries
      let url = fmap (fromJust . eURI) entry
      isInfix <- unwrapResult $ fmap (isInfixOf "nytimes") url
      isInfix `shouldSatisfy` isRight
    it "returns error when URL is not valid" $ do
      result <- unwrapResult $ fetch invalidEndPoint
      show result `shouldBe` "Left (FetchFeedError URLWrongFormat)"
    it "returns error when response can't be parsed to a feed" $ do
      result <- unwrapResult $ fetch simpleEndPoint
      show result `shouldBe` "Left (FetchFeedError FeedWrongFormat)"
    it "returns the http error when it happens" $ do
      Left (FetchFeedError (ResponseError (R.VanillaHttpException (H.HttpExceptionRequest _ (H.StatusCodeException response _))))) <-
        unwrapResult $ fetch (endPointWithStatus 404)
      H.responseStatus response `shouldBe` (toEnum 404)
