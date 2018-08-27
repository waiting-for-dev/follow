{-# LANGUAGE OverloadedStrings #-}

module FollowSpec where

import           Control.Monad.Except (runExceptT)
import           Data.Either          (fromRight)
import           Data.Text            (Text)
import qualified Data.Text            as T (concat)
import           Follow
import           Follow.Types         (Digester, Directory (..), Fetched,
                                       Middleware, Recipe (..), Result (..),
                                       Subject (..))
import           Test.Hspec

spec :: Spec
spec = do
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
