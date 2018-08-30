{-# LANGUAGE OverloadedStrings #-}

module Follow.Middlewares.FilterSpec where

import           Follow.Middlewares.Filter
import           Follow.Types              (Directory (..), Entry (..))
import           Helpers.Factories
import           Test.Hspec

spec :: Spec
spec =
  describe ".apply" $ do
    it "filter entries according to given predicate" $ do
      let entry1 = _entry {eTitle = Just "A"}
      let entry2 = _entry {eTitle = Just "B"}
      let directory = Directory _subject [entry1, entry2]
      let directory' = apply (eTitle `equalP` "A") directory
      dEntries directory' `shouldBe` [entry1]
