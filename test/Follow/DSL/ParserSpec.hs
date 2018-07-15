module Follow.DSL.ParserSpec where

import           Follow.DSL
import           Follow.Strategies.Null as Null (argumentsDSL)
import           Test.Hspec

spec :: Spec
spec = do
  describe ".parseDSL" $ do
    let success =
          unlines
            [ "VERSION 1.0"
            , "TITLE foo"
            , "DESCRIPTION description"
            , "TAGS taga, tagb"
            , "STRATEGY null"
            ]
    let (Right recipe) = parseDSL success Null.argumentsDSL
    it "copies VERSION value as version" $ rVersion recipe `shouldBe` "1.0"
    it "copies TITLE value as title" $ rTitle recipe `shouldBe` "foo"
    it "copies DESCRIPTION value as description" $
      rDescription recipe `shouldBe` "description"
    it "copies TAGS values as tags" $ rTags recipe `shouldBe` ["taga", "tagb"]
    it "extract strategy arguments from its specification" $
      rStrategyArguments recipe `shouldBe` []
