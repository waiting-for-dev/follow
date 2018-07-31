{-# LANGUAGE OverloadedStrings #-}

module Follow.FetchersSpec where

import           Data.Maybe      (fromJust, isNothing)
import           Follow.Fetchers
import           Follow.Types    (Directory (..), Entries, Entry (..),
                                  Recipe (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetch" $ do
    it "populates Directory Entries out of given fetcher" $ do
      let entries =
            [ Entry
                "http://url.com"
                "123"
                (Just "Title")
                (Just "Description")
                Nothing
            ]
      let fetcher = \_recipe -> return $ Just entries :: IO (Maybe Entries)
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      dEntries . fromJust <$> directory `shouldReturn` entries
    it "associates given recipe with the Directory" $ do
      let fetcher = \_recipe -> return $ Just [] :: IO (Maybe Entries)
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      rTitle <$> (dRecipe . fromJust <$> directory) `shouldReturn` "Title"
    it "sets Directory as Nothing when there is no entries" $ do
      let fetcher = \_recipe -> return Nothing :: IO (Maybe Entries)
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      isNothing <$> directory `shouldReturn` True
