{-# LANGUAGE OverloadedStrings #-}

module Follow.Fetchers.Feed.InternalSpec where

import qualified Data.ByteString               as BS (ByteString)
import           Data.Dynamic                  (toDyn)
import           Data.Either                   (isRight)
import           Data.Maybe                    (fromJust, isNothing)
import           Follow.Fetchers.Feed.Internal
import           Follow.Types                  (Entry (..))
import qualified Network.HTTP.Req              as R (parseUrl)
import           Test.Hspec
import qualified Text.Feed.Import              as FI (parseFeedFromFile)
import qualified Text.Feed.Types               as FT (Feed)

loadFeedFromFile :: IO FT.Feed
loadFeedFromFile = FI.parseFeedFromFile "test/Fixtures/rss.xml"

spec :: Spec
spec = do
  describe ".feedToEntries" $ do
    before loadFeedFromFile $ do
      it "adds an entry for each feed item" $ \feed -> do
        let entries = feedToEntries feed
        length entries `shouldBe` 2
      it "sets item link as entry uri" $ \feed -> do
        let entry = head $ feedToEntries feed
        eURI entry `shouldBe` Just "http://www.example.com/blog/post/1"
      it "sets item guid as entry id" $ \feed -> do
        let entry = head $ feedToEntries feed
        eGUID entry `shouldBe` Just "7bd204c6-1655-4c27-aeee-53f933c5395f"
      it "sets item title as entry title" $ \feed -> do
        let entry = head $ feedToEntries feed
        eTitle entry `shouldBe` Just "Example entry"
      it "sets item description as entry description" $ \feed -> do
        let entry = head $ feedToEntries feed
        eDescription entry `shouldBe` Just "Short description."
      it "sets item author as entry author" $ \feed -> do
        let entry = head $ feedToEntries feed
        eAuthor entry `shouldBe` Just "Joe Doe"
