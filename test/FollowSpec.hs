{-# LANGUAGE OverloadedStrings #-}

module FollowSpec where

import           Control.Monad.Except (throwError)
import           Data.Either          (fromRight)
import           Data.Text            (Text)
import qualified Data.Text            as T (concat)
import           Follow
import           Follow.Types         (Digester, Directory (..), Entry (..),
                                       FetchError (..), Fetched, Middleware,
                                       Recipe (..), Result, Subject (..))
import           Helpers.Factories
import           Test.Hspec

removeEntriesMiddleware = (\d -> Directory (dSubject d) []) :: Middleware

addEntryMiddleware entry =
  (\directory -> Directory (dSubject directory) $ entry : (dEntries directory)) :: Middleware

spec :: Spec
spec = do
  describe ".directoryFromFetched" $ do
    it "populates Directory Entries out of given fetcher" $ do
      let entry = _entry {eTitle = Just "Fetched"}
      let directory = directoryFromFetched (e2F entry) _subject
      fromDirEntries <- unwrapResult $ fmap dEntries directory
      fromRight [] fromDirEntries `shouldBe` [entry]
    it "associates given subject with the Directory" $ do
      let subject = _subject {sTitle = "DirectoryFromFetched"}
      let directory =
            directoryFromFetched (return [_entry] :: Fetched) $ subject
      fromDirSubject <- unwrapResult $ fmap dSubject directory
      fromRight _subject fromDirSubject `shouldBe` subject
    it "returns back any error from the fetcher" $ do
      let fetched = (throwError $ URLWrongFormat) :: Fetched
      result <- unwrapResult $ directoryFromFetched fetched _subject
      show result `shouldBe` "Left URLWrongFormat"
  describe ".applyMiddlewares" $ do
    it "applies in order given middlewares" $ do
      let directory = emptyDirectory _subject
      let middleware1 = addEntryMiddleware $ _entry {eTitle = Just "1"}
      let middleware2 = addEntryMiddleware $ _entry {eTitle = Just "2"}
      let result = applyMiddlewares [middleware1, middleware2] directory
      let resultEntries = dEntries result
      eTitle <$> resultEntries `shouldBe` [Just "2", Just "1"]
  describe ".directoryFromRecipe" $ do
    it "associates given subject to the created directory" $ do
      let recipe = Recipe _subject [] []
      Right directory <- unwrapResult $ directoryFromRecipe recipe
      dSubject directory `shouldBe` _subject
    it "concatenates given fetched entries" $ do
      let entry1 = _entry {eTitle = Just "Title 1"}
      let entry2 = _entry {eTitle = Just "Title 2"}
      let recipe = Recipe _subject [((e2F entry1), []), (e2F entry2, [])] []
      Right directory <- unwrapResult $ directoryFromRecipe recipe
      dEntries directory `shouldBe` [entry1, entry2]
    it "applies middlewares at each step" $ do
      let entry1 = _entry {eTitle = Just "Title 1"}
      let entry2 = _entry {eTitle = Just "Title 2"}
      let recipe =
            Recipe
              _subject
              [((e2F entry1), [removeEntriesMiddleware]), (e2F entry2, [])]
              []
      Right directory <- unwrapResult $ directoryFromRecipe recipe
      dEntries directory `shouldBe` [entry2]
    it "applies middlewares to fetched entries as a whole" $ do
      let entry1 = _entry {eTitle = Just "Title 1"}
      let recipe = Recipe _subject [(e2F entry1, [])] [removeEntriesMiddleware]
      Right directory <- unwrapResult $ directoryFromRecipe recipe
      dEntries directory `shouldBe` []
  describe ".applySteps" $ do
    let directory = emptyDirectory _subject
    it "concatenates given fetched entries" $ do
      let entry1 = _entry {eTitle = Just "Title 1"}
      let entry2 = _entry {eTitle = Just "Title 2"}
      Right directory <-
        unwrapResult $ applySteps directory [(e2F entry1, []), (e2F entry2, [])]
      dEntries directory `shouldBe` [entry1, entry2]
    it "applies middlewares at each step" $ do
      let entry1 = _entry {eTitle = Just "Title 1"}
      let entry2 = _entry {eTitle = Just "Title 2"}
      Right directory <-
        unwrapResult $
        applySteps
          directory
          [(e2F entry1, [removeEntriesMiddleware]), (e2F entry2, [])]
      dEntries directory `shouldBe` [entry2]
  describe ".emptyDirectory" $ do
    let directory = emptyDirectory _subject
    it "assigns given subject" $ do dSubject directory `shouldBe` _subject
    it "assigns empty list as entries" $ do dEntries directory `shouldBe` []
  describe ".mergeEntries" $ do
    let entry1 = _entry {eTitle = Just "1"}
    let entry2 = _entry {eTitle = Just "2"}
    let directory = Directory _subject [entry1]
    it "concatenate entries" $ do
      let directory' = mergeEntries directory [entry2]
      dEntries directory' `shouldBe` [entry1, entry2]
    it "keeps first appearance of duplicates" $ do
      let directory' = mergeEntries directory [entry2, entry1]
      dEntries directory' `shouldBe` [entry1, entry2]
