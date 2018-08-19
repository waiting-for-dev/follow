{-# LANGUAGE OverloadedStrings #-}

module Follow.FetchersSpec where

import           Control.Monad.Except (runExceptT, throwError)
import           Data.Either          (fromRight, isLeft)
import           Follow.Fetchers
import           Follow.Types         (Directory (..), Entry (..),
                                       FetchError (..), FetchFeedError (..),
                                       Fetcher, Recipe (..), Result (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetch" $ do
    let recipe = Recipe "1.0" "Title" "Description" ["tag"] []
    it "populates Directory Entries out of given fetcher" $ do
      let entries =
            [ Entry
                (Just "http://url.com")
                (Just "123")
                (Just "Title")
                (Just "Description")
                Nothing
            ]
      let fetcher = (\_recipe -> return entries) :: Fetcher
      let directory = fetch recipe fetcher
      fetchedEntries <- runExceptT (runResult $ fmap dEntries directory)
      fromRight [] fetchedEntries `shouldBe` entries
    it "associates given recipe with the Directory" $ do
      let fetcher = (\_recipe -> return []) :: Fetcher
      let directory = fetch recipe fetcher
      fetchedTitle <-
        runExceptT (runResult $ fmap rTitle (fmap dRecipe directory))
      fromRight "" fetchedTitle `shouldBe` "Title"
    it "returns back any error from the fetcher" $ do
      let fetcher =
            (\_recipe -> throwError $ FetchFeedError URLWrongFormat) :: Fetcher
      result <- runExceptT (runResult $ fetch recipe fetcher)
      show result `shouldBe` "Left (FetchFeedError URLWrongFormat)"
