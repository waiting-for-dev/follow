module Follow.DSL.ParserSpec where

import           Follow.DSL
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
    let (Right recipe) = parseDSL success
    it "copies VERSION value as version" $ rVersion recipe `shouldBe` "1.0"
    it "copies TITLE value as title" $ rTitle recipe `shouldBe` "foo"
    it "copies DESCRIPTION value as description" $
      rDescription recipe `shouldBe` "description"
    it "copies TAGS values as tags" $ rTags recipe `shouldBe` ["taga", "tagb"]
    it "copies STRATEGT value as strategt" $ rStrategy recipe `shouldBe` "null"
