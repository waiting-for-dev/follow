{-# LANGUAGE OverloadedStrings #-}

module Follow.Fetchers.FeedSpec where

import           Control.Monad.Except (runExceptT)
import qualified Data.ByteString      as BS (ByteString)
import           Data.Dynamic         (toDyn)
import           Data.Either          (isRight)
import           Data.Maybe           (fromJust)
import           Data.Text            (isInfixOf)
import           Follow.Fetchers.Feed
import           Follow.Types         (Entry (..), FetchError (..),
                                       FetchFeedError (..), Recipe (..),
                                       Result (..))
import qualified Network.HTTP.Client  as H (HttpException (..),
                                            HttpExceptionContent (..),
                                            responseStatus)
import qualified Network.HTTP.Req     as R (HttpException (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetcher" $ do
    let baseRecipe = Recipe "1.0" "Title" "Description" ["tag"]
    let buildRecipe url = baseRecipe [("URL", toDyn (url :: BS.ByteString))]
    it "fetches entries from given url" $ do
      let recipe =
            buildRecipe
              "http://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
      let entries = fetcher recipe
      let entry = fmap head entries
      let url = fmap (fromJust . eURI) entry
      isInfix <- runExceptT (runResult $ fmap (isInfixOf "nytimes") url)
      isInfix `shouldSatisfy` isRight
    it "returns error when URL is not encoded to ByteString" $ do
      let recipe = baseRecipe [("URL", toDyn ("http://url.com" :: String))]
      result <- runExceptT (runResult $ fetcher recipe)
      show result `shouldBe`
        "Left (FetchFeedError URLFromDynamicConversionFailure)"
    it "returns error when URL is not valid" $ do
      let recipe = buildRecipe "invalidUrl"
      result <- runExceptT (runResult $ fetcher recipe)
      show result `shouldBe` "Left (FetchFeedError URLWrongFormat)"
    it "returns error when response can't be parsed to a feed" $ do
      let recipe = buildRecipe "http://google.es"
      result <- runExceptT (runResult $ fetcher recipe)
      show result `shouldBe` "Left (FetchFeedError FeedWrongFormat)"
    it "returns the http error when it happens" $ do
      let recipe = buildRecipe "http://httpbin.org/status/404"
      Left (FetchFeedError (ResponseError (R.VanillaHttpException (H.HttpExceptionRequest _ (H.StatusCodeException response _))))) <-
        runExceptT (runResult $ fetcher recipe)
      H.responseStatus response `shouldBe` (toEnum 404)
