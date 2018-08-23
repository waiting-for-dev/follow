{-# LANGUAGE OverloadedStrings #-}

module Follow.FetchersSpec where

import           Control.Monad.Except (runExceptT, throwError)
import           Data.Either          (fromRight, isLeft)
import           Follow.Fetchers
import           Follow.Types         (Directory (..), Entry (..),
                                       FetchError (..), FetchFeedError (..),
                                       Fetched, Header (..), Result (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetch" $ do
    let header = Header "1.0" "Title" "Description" ["tag"] []
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
      let directory = fetch fetched header
      fetchedEntries <- runExceptT (runResult $ fmap dEntries directory)
      fromRight [] fetchedEntries `shouldBe` entries
    it "associates given header with the Directory" $ do
      let fetched = return [] :: Fetched
      let directory = fetch fetched header
      fetchedTitle <-
        runExceptT (runResult $ fmap hTitle (fmap dHeader directory))
      fromRight "" fetchedTitle `shouldBe` "Title"
    it "returns back any error from the fetcher" $ do
      let fetched = (throwError $ FetchFeedError URLWrongFormat) :: Fetched
      result <- runExceptT (runResult $ fetch fetched header)
      show result `shouldBe` "Left (FetchFeedError URLWrongFormat)"
