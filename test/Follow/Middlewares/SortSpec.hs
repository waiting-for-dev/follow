{-# LANGUAGE OverloadedStrings #-}

module Follow.Middlewares.SortSpec where

import           Follow.Middlewares.Sort
import           Follow.Types            (Directory (..), Entry (..))
import           Helpers.Factories
import           Test.Hspec

spec :: Spec
spec = do
  describe ".apply" $ do
    it "sorts by given field getter" $ do
      let entry1 = _entry {eTitle = Just "DEF"}
      let entry2 = _entry {eTitle = Just "ABC"}
      let directory = Directory _subject [entry1, entry2]
      let sortedDirectory = apply (byGetter eTitle) directory
      dEntries sortedDirectory `shouldBe` [entry2, entry1]
