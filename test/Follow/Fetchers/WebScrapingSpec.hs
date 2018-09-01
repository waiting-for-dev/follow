{-# LANGUAGE OverloadedStrings #-}

module Follow.Fetchers.WebScrapingSpec where

import           Data.Either                 (isRight)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (isInfixOf)
import           Follow.Fetchers.WebScraping
import           Follow.Types                (Entry (..), FetchError (..),
                                              unwrapResult)
import           Helpers.EndPointFixtures
import           Helpers.Factories
import qualified Network.HTTP.Client         as H (HttpException (..),
                                                   HttpExceptionContent (..),
                                                   responseStatus)
import qualified Network.HTTP.Req            as R (HttpException (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".fetch" $ do
    let selector = _selector {selURI = Just (Attr ".headline a" "href")}
    it "fetches entries from given url" $ do
      let entries = fetch (webScrapingEndPoint, selector)
      let entry = head <$> entries
      let url = fromJust . eURI <$> entry
      isInfix <- unwrapResult $ isInfixOf "nytimes" <$> url
      isInfix `shouldSatisfy` isRight
    it "returns error when URL is not valid" $ do
      result <- unwrapResult $ fetch (invalidEndPoint, selector)
      show result `shouldBe` "Left URLWrongFormat"
    it "returns the http error when it happens" $ do
      Left (ResponseError (R.VanillaHttpException (H.HttpExceptionRequest _ (H.StatusCodeException response _)))) <-
        unwrapResult $ fetch ((endPointWithStatus 404), selector)
      H.responseStatus response `shouldBe` (toEnum 404)
