{-# LANGUAGE OverloadedStrings #-}

module FollowSpec where

import           Control.Monad.Except (runExceptT, throwError)
import           Data.Either          (fromRight)
import           Data.Text            (Text)
import qualified Data.Text            as T (concat)
import           Follow
import           Follow.Types         (Digester, Directory (..), Entry (..),
                                       FetchError (..), FetchFeedError (..),
                                       Fetched, Middleware, Recipe (..),
                                       Result (..), Subject (..))
import           Test.Hspec

entryBuilder title = Entry Nothing Nothing (Just title) Nothing Nothing Nothing

fetcherBuilder title = (return $ [entryBuilder title]) :: Fetched

removeEntriesMiddleware = (\d -> Directory (dSubject d) []) :: Middleware

addEntryMiddleware entry =
  (\directory -> Directory (dSubject directory) $ entry : (dEntries directory)) :: Middleware

spec :: Spec
spec = do
  describe ".directoryFromFetched" $ do
    let subject = Subject "Title" "Description" ["tag"]
    it "populates Directory Entries out of given fetcher" $ do
      let entries =
            [ Entry
                (Just "http://url.com")
                (Just "123")
                (Just "Title")
                (Just "Description")
                Nothing
                Nothing
            ]
      let fetched = return entries :: Fetched
      let directory = directoryFromFetched fetched subject
      fetchedEntries <- runExceptT (runResult $ fmap dEntries directory)
      fromRight [] fetchedEntries `shouldBe` entries
    it "associates given subject with the Directory" $ do
      let fetched = return [] :: Fetched
      let directory = directoryFromFetched fetched subject
      fetchedTitle <-
        runExceptT (runResult $ fmap sTitle (fmap dSubject directory))
      fromRight "" fetchedTitle `shouldBe` "Title"
    it "returns back any error from the fetcher" $ do
      let fetched = (throwError $ FetchFeedError URLWrongFormat) :: Fetched
      result <- runExceptT (runResult $ directoryFromFetched fetched subject)
      show result `shouldBe` "Left (FetchFeedError URLWrongFormat)"
  describe ".applyMiddlewares" $ do
    it "applies in order given middlewares" $ do
      let subject = Subject "Title" "Desc" ["tag"]
      let directory = Directory subject []
      let middleware1 = addEntryMiddleware $ entryBuilder "1"
      let middleware2 = addEntryMiddleware $ entryBuilder "2"
      let result = applyMiddlewares [middleware1, middleware2] directory
      let resultEntries = dEntries result
      eTitle <$> resultEntries `shouldBe` [Just "2", Just "1"]
  describe ".directoryFromRecipe" $ do
    let subject = Subject "Title" "Description" ["tag"]
    it "associates given subject to the created directory" $ do
      let recipe = Recipe subject [] []
      Right directory <- runExceptT (runResult $ directoryFromRecipe recipe)
      dSubject directory `shouldBe` subject
    it "concatenates given fetched entries" $ do
      let fetched1 = fetcherBuilder "Title 1"
      let fetched2 = fetcherBuilder "Title 2"
      let recipe = Recipe subject [(fetched1, []), (fetched2, [])] []
      Right directory <- runExceptT (runResult $ directoryFromRecipe recipe)
      eTitle <$> dEntries directory `shouldBe` [Just "Title 1", Just "Title 2"]
    it "applies middlewares at each step" $ do
      let fetched1 = fetcherBuilder "Title 1"
      let fetched2 = fetcherBuilder "Title 2"
      let recipe =
            Recipe
              subject
              [(fetched1, [removeEntriesMiddleware]), (fetched2, [])]
              []
      Right directory <- runExceptT (runResult $ directoryFromRecipe recipe)
      eTitle <$> dEntries directory `shouldBe` [Just "Title 2"]
    it "applies middlewares to fetched entries as a whole" $ do
      let fetched1 = fetcherBuilder "Title 1"
      let recipe = Recipe subject [(fetched1, [])] [removeEntriesMiddleware]
      Right directory <- runExceptT (runResult $ directoryFromRecipe recipe)
      dEntries directory `shouldBe` []
  describe ".applySteps" $ do
    let subject = Subject "Title" "Description" ["tag"]
    let directory = emptyDirectory subject
    it "concatenates given fetched entries" $ do
      let fetched1 = fetcherBuilder "Title 1"
      let fetched2 = fetcherBuilder "Title 2"
      Right directory <-
        runExceptT
          (runResult $ applySteps directory [(fetched1, []), (fetched2, [])])
      eTitle <$> dEntries directory `shouldBe` [Just "Title 1", Just "Title 2"]
    it "applies middlewares at each step" $ do
      let fetched1 = fetcherBuilder "Title 1"
      let fetched2 = fetcherBuilder "Title 2"
      Right directory <-
        runExceptT
          (runResult $
           applySteps
             directory
             [(fetched1, [removeEntriesMiddleware]), (fetched2, [])])
      eTitle <$> dEntries directory `shouldBe` [Just "Title 2"]
  describe ".emptyDirectory" $ do
    let subject = Subject "Title" "Description" ["tag"]
    let directory = emptyDirectory subject
    it "assigns given subject" $ do dSubject directory `shouldBe` subject
    it "assigns empty list as entries" $ do dEntries directory `shouldBe` []
  describe ".mergeEntries" $ do
    let subject = Subject "Title" "Description" ["tag"]
    let entry1 = entryBuilder "1"
    let entry2 = entryBuilder "2"
    let directory = Directory subject [entry1]
    it "concatenate entries" $ do
      let directory' = mergeEntries directory [entry2]
      dEntries directory' `shouldBe` [entry1, entry2]
    it "keeps first appearance of duplicates" $ do
      let directory' = mergeEntries directory [entry2, entry1]
      dEntries directory' `shouldBe` [entry1, entry2]
