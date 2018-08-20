{-# LANGUAGE OverloadedStrings #-}

module Follow.Digesters.SimpleTextSpec where

import qualified Data.Text                   as T (isInfixOf)
import           Follow.Digesters.SimpleText
import           Follow.Types                (Directory (..), Entry (..),
                                              Recipe (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".digester" $ do
    let isInfixOf' = flip T.isInfixOf
    let recipe = Recipe "1.0" "Recipe Title" "Recipe Description" ["tag"] []
    let entries =
          [ Entry
              (Just "http://a_url.com")
              (Just "a_guid")
              (Just "Entry Title")
              (Just "Entry Description")
              (Just "Entry Author")
          ]
    let directory = Directory recipe entries
    let output = digester directory
    it "adds recipe title" $ do
      "Recipe Title" `shouldSatisfy` (isInfixOf' output)
    it "adds recipe description" $ do
      "Recipe Description" `shouldSatisfy` (isInfixOf' output)
    it "adds recipe tags" $ do "tag" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry URL" $ do
      "http://a_url.com" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry title" $ do
      "Entry Title" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry description" $ do
      "Entry Description" `shouldSatisfy` (isInfixOf' output)
    it "adds each entry author" $ do
      "Entry Author" `shouldSatisfy` (isInfixOf' output)
