module Follow.DSLParserSpec where

import Test.Hspec

import Follow.DSLParser

spec :: Spec
spec = do
  describe ".parse" $ do
    it "copies FOLLOW value as title" $ do
      let (Right parsed) = parseDSL("FOLLOW foo")
      parsedDSLTitle parsed `shouldBe` "foo"
