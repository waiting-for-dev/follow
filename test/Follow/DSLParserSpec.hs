module Follow.DSLParserSpec where

import           Test.Hspec

import           Follow.DSLParser

spec :: Spec
spec = do
  describe ".parseDSL" $ do
    let success =
          unlines ["VERSION 1.0", "FOLLOW foo", "DESCRIBED BY description"]
    it "copies VERSION value as value" $ do
      let (Right recipe) = parseDSL success
      rVersion recipe `shouldBe` "1.0"
    it "copies FOLLOW value as title" $ do
      let (Right recipe) = parseDSL success
      rTitle recipe `shouldBe` "foo"
    it "copies DESCRIBED BY value as description" $ do
      let (Right recipe) = parseDSL success
      rDescription recipe `shouldBe` "description"
