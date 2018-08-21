{-# LANGUAGE OverloadedStrings #-}

module Follow.Middlewares.Filter.InternalSpec where

import           Follow.Middlewares.Filter.Internal
import           Follow.Types                       (Entry (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".equalP" $ do
    it "returns true on equality" $ do
      let entry = Entry (Just "http://url.com") Nothing Nothing Nothing Nothing
      (eURI `equalP` "http://url.com") entry `shouldBe` True
    it "returns false on inequality" $ do
      let entry = Entry Nothing (Just "GUID1") Nothing Nothing Nothing
      (eGUID `equalP` "GUID2") entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = Entry Nothing Nothing Nothing Nothing Nothing
      (eTitle `equalP` "Title") entry `shouldBe` False
