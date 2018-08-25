{-# LANGUAGE OverloadedStrings #-}

module Follow.FetchersSpec where

import           Control.Monad.Except (runExceptT, throwError)
import           Data.Either          (fromRight, isLeft)
import           Follow.Fetchers
import           Follow.Types         (Directory (..), Entry (..),
                                       FetchError (..), FetchFeedError (..),
                                       Fetched, Result (..), Subject (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".buildDirectory" $ do
    let subject = Subject "Title" "Description" ["tag"]
    it "populates Directory Entries out of given fetcher" $ do
      let entries =
            [ Entry
                (Just "http://url.com")
                (Just "123")
                (Just "Title")
                (Just "Description")
                Nothing
            ]
      let fetched = return entries :: Fetched
      let directory = buildDirectory fetched subject
      fetchedEntries <- runExceptT (runResult $ fmap dEntries directory)
      fromRight [] fetchedEntries `shouldBe` entries
    it "associates given subject with the Directory" $ do
      let fetched = return [] :: Fetched
      let directory = buildDirectory fetched subject
      fetchedTitle <-
        runExceptT (runResult $ fmap sTitle (fmap dSubject directory))
      fromRight "" fetchedTitle `shouldBe` "Title"
    it "returns back any error from the fetcher" $ do
      let fetched = (throwError $ FetchFeedError URLWrongFormat) :: Fetched
      result <- runExceptT (runResult $ buildDirectory fetched subject)
      show result `shouldBe` "Left (FetchFeedError URLWrongFormat)"
