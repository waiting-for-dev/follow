module Follow.DSL.ParserSpec where

import           Follow.DSL
import           Test.Hspec

spec :: Spec
spec = do
  describe ".parseDSL" $ do
    let success =
          unlines ["VERSION 1.0", "TITLE foo", "DESCRIPTION description"]
    let (Right recipe) = parseDSL success
    it "copies VERSION value as version" $ rVersion recipe `shouldBe` "1.0"
    it "copies TITLE value as title" $ rTitle recipe `shouldBe` "foo"
    it "copies DESCRIPTION value as description" $
      rDescription recipe `shouldBe` "description"
