{-# LANGUAGE OverloadedStrings #-}

module Follow.Middlewares.Filter.InternalSpec where

import           Follow.Middlewares.Filter.Internal
import           Follow.Types                       (Entry (..))
import           Test.Hspec

spec :: Spec
spec = do
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
  describe ".containP" $ do
    it "returns true when needle is contained" $ do
      let entry =
            Entry
              (Just "http://this_is_a_url.com")
              Nothing
              Nothing
              Nothing
              Nothing
      (eURI `containP` "url") entry `shouldBe` True
    it "returns false when needle is not contained" $ do
      let entry = Entry Nothing (Just "GUID1") Nothing Nothing Nothing
      (eGUID `containP` "NO") entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = Entry Nothing Nothing Nothing Nothing Nothing
      (eTitle `containP` "Title") entry `shouldBe` False
  describe ".prefixP" $ do
    it "returns true when needle is prefix" $ do
      let entry = Entry (Just "http://url.com") Nothing Nothing Nothing Nothing
      (eURI `prefixP` "http://") entry `shouldBe` True
    it "returns false when needle is not prefix" $ do
      let entry = Entry Nothing (Just "GUID1") Nothing Nothing Nothing
      (eGUID `prefixP` "NO") entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = Entry Nothing Nothing Nothing Nothing Nothing
      (eTitle `prefixP` "Title") entry `shouldBe` False
  describe ".suffixP" $ do
    it "returns true when needle is suffix" $ do
      let entry = Entry (Just "http://url.com") Nothing Nothing Nothing Nothing
      (eURI `suffixP` ".com") entry `shouldBe` True
    it "returns false when needle is not suffix" $ do
      let entry = Entry Nothing (Just "GUID1") Nothing Nothing Nothing
      (eGUID `suffixP` "NO") entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = Entry Nothing Nothing Nothing Nothing Nothing
      (eTitle `suffixP` "Title") entry `shouldBe` False
