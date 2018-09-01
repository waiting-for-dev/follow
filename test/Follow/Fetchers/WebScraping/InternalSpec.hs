{-# LANGUAGE OverloadedStrings #-}

module Follow.Fetchers.WebScraping.InternalSpec where

import           Data.ByteString.Lazy                 as BL (ByteString)
import           Data.ByteString.Lazy.Char8           as BL (pack)
import           Follow.Fetchers.WebScraping.Internal
import           Follow.Types                         (Entry (..))
import           Test.Hspec

loadHtmlFromFile :: IO BL.ByteString
loadHtmlFromFile = BL.pack <$> readFile "test/Fixtures/webscraping.html"

spec :: Spec
spec =
  describe ".htmlToEntries" $ do
    let selector =
          Selector
            { selURI = Just $ Attr "article a" "href"
            , selGUID = Just $ Attr "article a" "href"
            , selTitle = Just $ InnerText ".title a"
            , selDescription = Just $ InnerText ".description"
            , selAuthor = Just $ InnerText ".author"
            , selPublishDate = Just $ Attr ".publish-date" "datetime"
            }
    before loadHtmlFromFile $ do
      it "creates as many entries as links" $ \html -> do
        entries <- htmlToEntries html selector
        length entries `shouldBe` 2
      it "uses URI selector to parse URIs" $ \html -> do
        entry <- head <$> htmlToEntries html selector
        eURI entry `shouldBe` Just "http://sampleblog.com/first-article"
      it "uses GUID selector to parse GUIDs" $ \html -> do
        entry <- head <$> htmlToEntries html selector
        eGUID entry `shouldBe` Just "http://sampleblog.com/first-article"
      it "uses title selector to parse titles" $ \html -> do
        entry <- head <$> htmlToEntries html selector
        eTitle entry `shouldBe` Just "First article"
      it "uses description selector to parse descriptions" $ \html -> do
        entry <- head <$> htmlToEntries html selector
        eDescription entry `shouldBe` Just "Description of the first article"
      it "uses publish date selector to parse publish dates" $ \html -> do
        entry <- head <$> htmlToEntries html selector
        (show <$> ePublishDate entry) `shouldBe` Just "2018-08-31 03:00:06 UTC"
      it "fills unmatched with Nothing" $ \html -> do
        entry <- head . tail <$> htmlToEntries html selector
        ePublishDate entry `shouldBe` Nothing
