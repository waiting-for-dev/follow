{-# LANGUAGE OverloadedStrings #-}

module Follow.Fetchers.FeedSpec where

import           Control.Monad.Except (runExceptT)
import qualified Data.ByteString      as BS (ByteString)
import           Data.Dynamic         (toDyn)
import           Data.Either          (isRight)
import           Data.Maybe           (fromJust)
import           Data.Text            (isInfixOf)
import           Follow.Fetchers.Feed
import           Follow.Types         (Entry (..), Recipe (..), Result (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetcher" $ do
    it "fetches entries from given url" $ do
      let recipe =
            Recipe
              "1.0"
              "Title"
              "Description"
              ["tag"]
              [ ( "URL"
                , toDyn
                    ("http://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml" :: BS.ByteString))
              ]
      let entries = fetcher recipe
      let entry = fmap head entries
      let url = fmap (fromJust . eURI) entry
      let isInfix = runExceptT (runResult $ fmap (isInfixOf "nytimes") url)
      isRight <$> isInfix `shouldReturn` True
