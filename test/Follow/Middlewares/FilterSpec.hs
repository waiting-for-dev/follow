{-# LANGUAGE OverloadedStrings #-}

module Follow.Middlewares.FilterSpec where

import           Follow.Middlewares.Filter
import           Follow.Middlewares.Filter.Internal
import           Follow.Types                       (Directory (..), Entry (..),
                                                     Header (..), Middleware)
import           Test.Hspec

spec :: Spec
spec =
  describe ".apply" $ do
    let buildEntry title = Entry Nothing Nothing (Just title) Nothing Nothing
    it "filter entries according to given predicate" $ do
      let header = Header "Title" "Description" ["tag"]
      let directory = Directory header [buildEntry "A", buildEntry "B"]
      let directory' = apply (eTitle `equalP` "A") directory
      (length $ dEntries directory') `shouldBe` 1
      eTitle (head $ dEntries directory') `shouldBe` Just "A"
