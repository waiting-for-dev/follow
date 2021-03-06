{-# LANGUAGE OverloadedStrings #-}

module Follow.Digesters.SimpleTextSpec where

import qualified Data.Text                   as T (isInfixOf)
import           Data.Time                   (LocalTime)
import           Follow.Digesters.SimpleText
import           Follow.Types                (Directory (..), Entry (..),
                                              Subject (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".digest" $ do
    let isInfixOf' = flip T.isInfixOf
    let subject = Subject "Subject Title" "Subject Description" ["tag"]
    let entries =
          [ Entry
              (Just "http://a_url.com")
              (Just "a_guid")
              (Just "Entry Title")
              (Just "Entry Description")
              (Just "Entry Author")
              (Just (read "2009-09-06 16:20:00" :: LocalTime))
          ]
    let directory = Directory subject entries
    let output = digest directory
    it "adds subject title" $ do
      "Subject Title" `shouldSatisfy` (isInfixOf' output)
    it "adds subject description" $ do
      "Subject Description" `shouldSatisfy` (isInfixOf' output)
    it "adds subject tags" $ do "tag" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry URL" $ do
      "http://a_url.com" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry title" $ do
      "Entry Title" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry description" $ do
      "Entry Description" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry author" $ do
      "Entry Author" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry publish date" $ do
      "2009-09-06 16:20:00" `shouldSatisfy` (isInfixOf' output)
