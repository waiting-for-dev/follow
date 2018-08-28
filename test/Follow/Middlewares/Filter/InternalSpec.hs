{-# LANGUAGE OverloadedStrings #-}

module Follow.Middlewares.Filter.InternalSpec where

import           Follow.Middlewares.Filter.Internal
import           Follow.Types                       (Entry (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".equalP" $ do
    it "returns true on equality" $ do
      let entry =
            Entry
              (Just "http://url.com")
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
      (eURI `equalP` "http://url.com") entry `shouldBe` True
    it "returns false on inequality" $ do
      let entry = Entry Nothing (Just "GUID1") Nothing Nothing Nothing Nothing
      (eGUID `equalP` "GUID2") entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = Entry Nothing Nothing Nothing Nothing Nothing Nothing
      (eTitle `equalP` "Title") entry `shouldBe` False
  describe ".infixP" $ do
    it "returns true when needle is contained" $ do
      let entry =
            Entry
              (Just "http://this_is_a_url.com")
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
      ("url" `infixP` eURI) entry `shouldBe` True
    it "returns false when needle is not contained" $ do
      let entry = Entry Nothing (Just "GUID1") Nothing Nothing Nothing Nothing
      ("NO" `infixP` eGUID) entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = Entry Nothing Nothing Nothing Nothing Nothing Nothing
      ("Title" `infixP` eTitle) entry `shouldBe` False
  describe ".prefixP" $ do
    it "returns true when needle is prefix" $ do
      let entry =
            Entry
              (Just "http://url.com")
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
      ("http://" `prefixP` eURI) entry `shouldBe` True
    it "returns false when needle is not prefix" $ do
      let entry = Entry Nothing (Just "GUID1") Nothing Nothing Nothing Nothing
      ("NO" `prefixP` eGUID) entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = Entry Nothing Nothing Nothing Nothing Nothing Nothing
      ("Title" `prefixP` eTitle) entry `shouldBe` False
  describe ".suffixP" $ do
    it "returns true when needle is suffix" $ do
      let entry =
            Entry
              (Just "http://url.com")
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
      (".com" `suffixP` eURI) entry `shouldBe` True
    it "returns false when needle is not suffix" $ do
      let entry = Entry Nothing (Just "GUID1") Nothing Nothing Nothing Nothing
      ("NO" `suffixP` eGUID) entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = Entry Nothing Nothing Nothing Nothing Nothing Nothing
      ("Title" `suffixP` eTitle) entry `shouldBe` False
  describe ".lessP" $ do
    it "returns true when item is lesser" $ do
      let entry = Entry Nothing Nothing (Just "A") Nothing Nothing Nothing
      (eTitle `lessP` "B") entry `shouldBe` True
    it "returns false when item is not lesser" $ do
      let entry = Entry Nothing Nothing (Just "C") Nothing Nothing Nothing
      (eTitle `lessP` "B") entry `shouldBe` False
    it "returns false when item is equal" $ do
      let entry = Entry Nothing Nothing (Just "A") Nothing Nothing Nothing
      (eTitle `lessP` "A") entry `shouldBe` False
  describe ".greaterP" $ do
    it "returns true when item is greater" $ do
      let entry = Entry Nothing Nothing (Just "C") Nothing Nothing Nothing
      (eTitle `greaterP` "A") entry `shouldBe` True
    it "returns false when item is not greater" $ do
      let entry = Entry Nothing Nothing (Just "A") Nothing Nothing Nothing
      (eTitle `greaterP` "B") entry `shouldBe` False
    it "returns false when item is equal" $ do
      let entry = Entry Nothing Nothing (Just "A") Nothing Nothing Nothing
      (eTitle `greaterP` "A") entry `shouldBe` False
  describe ".andP" $ do
    it "returns true when both predicates apply" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      (("The" `prefixP` eTitle) `andP` ("Title" `suffixP` eTitle)) entry `shouldBe`
        True
    it "returns false when first predicates fails" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      (("NO" `prefixP` eTitle) `andP` ("Title" `suffixP` eTitle)) entry `shouldBe`
        False
    it "returns false when second predicates fails" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      (("The" `prefixP` eTitle) `andP` ("NO" `suffixP` eTitle)) entry `shouldBe`
        False
    it "returns false when both predicates fail" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      (("NO" `prefixP` eTitle) `andP` ("NEITHER" `suffixP` eTitle)) entry `shouldBe`
        False
  describe ".orP" $ do
    it "returns true when both predicates apply" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      (("The" `prefixP` eTitle) `orP` ("Title" `suffixP` eTitle)) entry `shouldBe`
        True
    it "returns true when first predicates applies" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      (("The" `prefixP` eTitle) `orP` ("NO" `suffixP` eTitle)) entry `shouldBe`
        True
    it "returns true when second predicates applies" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      (("NO" `prefixP` eTitle) `orP` ("Title" `suffixP` eTitle)) entry `shouldBe`
        True
    it "returns false when both predicates fail" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      (("NO" `prefixP` eTitle) `orP` ("NEITHER" `suffixP` eTitle)) entry `shouldBe`
        False
  describe ".notP" $ do
    it "returns true when predicate does not apply" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      notP ("NO" `prefixP` eTitle) entry `shouldBe` True
    it "returns false when predicate applies" $ do
      let entry =
            Entry Nothing Nothing (Just "The Title") Nothing Nothing Nothing
      notP ("The" `prefixP` eTitle) entry `shouldBe` False
