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
  describe ".applyMiddlewares" $ do
    let buildEntry title =
          Entry Nothing (Just title) (Just title) Nothing Nothing
    let addEntryMiddleware entry =
          (\directory ->
             Directory (dSubject directory) $ entry : (dEntries directory)) :: Middleware
    it "applies in order given middlewares" $ do
      let subject = Subject "Title" "Desc" ["tag"]
      let directory = Directory subject []
      let middleware1 = addEntryMiddleware $ buildEntry "A"
      let middleware2 = addEntryMiddleware $ buildEntry "B"
      let result = applyMiddlewares [middleware1, middleware2] directory
      let resultEntries = dEntries result
      eTitle <$> resultEntries `shouldBe` [Just "B", Just "A"]
  describe ".process" $ do
    it "fetches, applies middlewares and digests using given strategies" $ do
      let subject = Subject "Title" "Description" ["tag"]
      let fetched = return [] :: Fetched
      let middleware =
            (\directory ->
               directory {dSubject = subject {sTitle = "Title updated"}}) :: Middleware
      let digester =
            (\directory ->
               case dEntries directory of
                 [] ->
                   T.concat [sTitle $ dSubject directory, " // Empty Entries"]
                 _ -> "Full Entries") :: Digester Text
      result <-
        runExceptT (runResult $ process fetched [middleware] digester subject)
      fromRight "" result `shouldBe` "Title updated // Empty Entries"
  describe ".processRecipe" $ do
    let subject = Subject "Title" "Description" ["tag"]
    let fetcherBuilder title =
          return [Entry Nothing Nothing (Just title) Nothing Nothing] :: Fetched
    let removeEntriesMiddleware =
          (\d -> Directory (dSubject d) []) :: Middleware
    it "associates given subject to the created directory" $ do
      let recipe = Recipe subject [] []
      Right directory <- runExceptT (runResult $ processRecipe recipe)
      dSubject directory `shouldBe` subject
    it "concatenates given fetched entries" $ do
      let fetched1 = fetcherBuilder "Title 1"
      let fetched2 = fetcherBuilder "Title 2"
      let recipe = Recipe subject [(fetched1, []), (fetched2, [])] []
      Right directory <- runExceptT (runResult $ processRecipe recipe)
      eTitle <$> dEntries directory `shouldBe` [Just "Title 1", Just "Title 2"]
    it "applies middlewares at each step" $ do
      let fetched1 = fetcherBuilder "Title 1"
      let fetched2 = fetcherBuilder "Title 2"
      let recipe =
            Recipe
              subject
              [(fetched1, [removeEntriesMiddleware]), (fetched2, [])]
              []
      Right directory <- runExceptT (runResult $ processRecipe recipe)
      eTitle <$> dEntries directory `shouldBe` [Just "Title 2"]
    it "applies middlewares to fetched entries as a whole" $ do
      let fetched1 = fetcherBuilder "Title 1"
      let recipe = Recipe subject [(fetched1, [])] [removeEntriesMiddleware]
      Right directory <- runExceptT (runResult $ processRecipe recipe)
      dEntries directory `shouldBe` []
