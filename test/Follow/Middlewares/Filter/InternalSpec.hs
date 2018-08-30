{-# LANGUAGE OverloadedStrings #-}

module Follow.Middlewares.Filter.InternalSpec where

import           Follow.Middlewares.Filter.Internal
import           Follow.Types                       (Entry (..))
import           Helpers.Factories
import           Test.Hspec

spec :: Spec
spec = do
  describe ".equalP" $ do
    it "returns true on equality" $ do
      let entry = _entry {eURI = Just "http://url.com"}
      (eURI `equalP` "http://url.com") entry `shouldBe` True
    it "returns false on inequality" $ do
      let entry = _entry {eGUID = Just "GUID1"}
      (eGUID `equalP` "GUID2") entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = _entry {eTitle = Nothing}
      (eTitle `equalP` "Title") entry `shouldBe` False
  describe ".infixP" $ do
    it "returns true when needle is contained" $ do
      let entry = _entry {eURI = Just "http://this_is_a_url.com"}
      ("url" `infixP` eURI) entry `shouldBe` True
    it "returns false when needle is not contained" $ do
      let entry = _entry {eGUID = Just "GUID1"}
      ("NO" `infixP` eGUID) entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = _entry {eTitle = Nothing}
      ("Title" `infixP` eTitle) entry `shouldBe` False
  describe ".prefixP" $ do
    it "returns true when needle is prefix" $ do
      let entry = _entry {eURI = Just "http://url.com"}
      ("http://" `prefixP` eURI) entry `shouldBe` True
    it "returns false when needle is not prefix" $ do
      let entry = _entry {eGUID = Just "GUID1"}
      ("NO" `prefixP` eGUID) entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = _entry {eTitle = Nothing}
      ("Title" `prefixP` eTitle) entry `shouldBe` False
  describe ".suffixP" $ do
    it "returns true when needle is suffix" $ do
      let entry = _entry {eURI = Just "http://url.com"}
      (".com" `suffixP` eURI) entry `shouldBe` True
    it "returns false when needle is not suffix" $ do
      let entry = _entry {eGUID = Just "GUID1"}
      ("NO" `suffixP` eGUID) entry `shouldBe` False
    it "returns false when field is Nothing" $ do
      let entry = _entry {eTitle = Nothing}
      ("Title" `suffixP` eTitle) entry `shouldBe` False
  describe ".lessP" $ do
    it "returns true when item is lesser" $ do
      let entry = _entry {eTitle = Just "A"}
      (eTitle `lessP` "B") entry `shouldBe` True
    it "returns false when item is not lesser" $ do
      let entry = _entry {eTitle = Just "C"}
      (eTitle `lessP` "B") entry `shouldBe` False
    it "returns false when item is equal" $ do
      let entry = _entry {eTitle = Just "A"}
      (eTitle `lessP` "A") entry `shouldBe` False
    it "returns false when item is Nothing" $ do
      let entry = _entry {eTitle = Nothing}
      (eTitle `lessP` "A") entry `shouldBe` False
  describe ".greaterP" $ do
    it "returns true when item is greater" $ do
      let entry = _entry {eTitle = Just "C"}
      (eTitle `greaterP` "A") entry `shouldBe` True
    it "returns false when item is not greater" $ do
      let entry = _entry {eTitle = Just "A"}
      (eTitle `greaterP` "B") entry `shouldBe` False
    it "returns false when item is equal" $ do
      let entry = _entry {eTitle = Just "A"}
      (eTitle `greaterP` "A") entry `shouldBe` False
    it "returns false when item is Nothing" $ do
      let entry = _entry {eTitle = Nothing}
      (eTitle `greaterP` "A") entry `shouldBe` False
  describe ".andP" $ do
    let entry = _entry {eTitle = Just "The Title"}
    it "returns true when both predicates apply" $ do
      (("The" `prefixP` eTitle) `andP` ("Title" `suffixP` eTitle)) entry `shouldBe`
        True
    it "returns false when first predicates fails" $ do
      (("NO" `prefixP` eTitle) `andP` ("Title" `suffixP` eTitle)) entry `shouldBe`
        False
    it "returns false when second predicates fails" $ do
      (("The" `prefixP` eTitle) `andP` ("NO" `suffixP` eTitle)) entry `shouldBe`
        False
    it "returns false when both predicates fail" $ do
      (("NO" `prefixP` eTitle) `andP` ("NEITHER" `suffixP` eTitle)) entry `shouldBe`
        False
  describe ".orP" $ do
    let entry = _entry {eTitle = Just "The Title"}
    it "returns true when both predicates apply" $ do
      (("The" `prefixP` eTitle) `orP` ("Title" `suffixP` eTitle)) entry `shouldBe`
        True
    it "returns true when first predicates applies" $ do
      (("The" `prefixP` eTitle) `orP` ("NO" `suffixP` eTitle)) entry `shouldBe`
        True
    it "returns true when second predicates applies" $ do
      (("NO" `prefixP` eTitle) `orP` ("Title" `suffixP` eTitle)) entry `shouldBe`
        True
    it "returns false when both predicates fail" $ do
      (("NO" `prefixP` eTitle) `orP` ("NEITHER" `suffixP` eTitle)) entry `shouldBe`
        False
  describe ".notP" $ do
    let entry = _entry {eTitle = Just "The Title"}
    it "returns true when predicate does not apply" $ do
      notP ("NO" `prefixP` eTitle) entry `shouldBe` True
    it "returns false when predicate applies" $ do
      notP ("The" `prefixP` eTitle) entry `shouldBe` False
