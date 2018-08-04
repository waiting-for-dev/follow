{-# LANGUAGE OverloadedStrings #-}

module Follow.FetchersSpec where

import           Data.Either     (isLeft)
import           Follow.Fetchers
import           Follow.Types    (Directory (..), Entries, Entry (..), Fetcher,
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
      let fetcher = (\_recipe -> return $ Right entries) :: Fetcher
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      fmap dEntries <$> directory `shouldReturn` Right entries
    it "associates given recipe with the Directory" $ do
      let fetcher = (\_recipe -> return $ Right []) :: Fetcher
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      fmap rTitle <$> (fmap dRecipe <$> directory) `shouldReturn` Right "Title"
    it "returns back any error from the fetcher" $ do
      let fetcher = (\_recipe -> return $ Left "Error") :: Fetcher
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      isLeft <$> directory `shouldReturn` True
