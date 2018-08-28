{-# LANGUAGE OverloadedStrings #-}

module Follow.Middlewares.SortSpec where

import           Follow.Middlewares.Sort
import           Follow.Types            (Directory (..), Entry (..),
                                          Subject (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".apply" $ do
    it "sorts by given field getter" $ do
      let subject = Subject "Title" "Description" ["tag"]
      let entry1 = Entry Nothing Nothing (Just "DEF") Nothing Nothing
      let entry2 = Entry Nothing Nothing (Just "ABC") Nothing Nothing
      let directory = Directory subject [entry1, entry2]
      let sortedDirectory = apply (byGetter eTitle) directory
      dEntries sortedDirectory `shouldBe` [entry2, entry1]
