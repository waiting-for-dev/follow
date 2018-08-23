{-# LANGUAGE OverloadedStrings #-}

module Follow.Digesters.SimpleTextSpec where

import qualified Data.Text                   as T (isInfixOf)
import           Follow.Digesters.SimpleText
import           Follow.Types                (Directory (..), Entry (..),
                                              Header (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".digester" $ do
    let isInfixOf' = flip T.isInfixOf
    let header = Header "Header Title" "Header Description" ["tag"]
    let entries =
          [ Entry
              (Just "http://a_url.com")
              (Just "a_guid")
              (Just "Entry Title")
              (Just "Entry Description")
              (Just "Entry Author")
          ]
    let directory = Directory header entries
    let output = digester directory
    it "adds header title" $ do
      "Header Title" `shouldSatisfy` (isInfixOf' output)
    it "adds header description" $ do
      "Header Description" `shouldSatisfy` (isInfixOf' output)
    it "adds header tags" $ do "tag" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry URL" $ do
      "http://a_url.com" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry title" $ do
      "Entry Title" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry description" $ do
      "Entry Description" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry author" $ do
      "Entry Author" `shouldSatisfy` (isInfixOf' output)
