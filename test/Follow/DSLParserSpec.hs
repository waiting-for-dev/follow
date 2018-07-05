module Follow.DSLParserSpec where

import Test.Hspec

import Follow.DSLParser

spec :: Spec
spec = do
  describe ".parseDSL" $ do
    let success = unlines [
            "VERSION 1.0"
          , "FOLLOW foo"
          , "DESCRIBED BY description"]
    
    it "copies VERSION value as value" $ do
      let (Right parsed) = parseDSL success
      parsedDSLVersion parsed `shouldBe` "1.0"

    it "copies FOLLOW value as title" $ do
      let (Right parsed) = parseDSL success
      parsedDSLTitle parsed `shouldBe` "foo"

    it "copies DESCRIBED BY value as description" $ do
      let (Right parsed) = parseDSL success
      parsedDSLDescription parsed `shouldBe` "description"
