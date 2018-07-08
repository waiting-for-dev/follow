module Follow.DSL.FormatSpec where

import           Follow.DSL.Format
import           Test.Hspec
import           Text.Parsec       (parse)

spec :: Spec
spec = do
  describe ".format" $ do
    let success =
          unlines ["VERSION 1.0", "TITLE foo", "DESCRIPTION description"]
    let (Right (version, title, description)) = parse format "test" success
    it "defines VERSION in the first line" $ do version `shouldBe` "1.0"
    it "defines TITLE in the second line" $ do title `shouldBe` "foo"
    it "defines DESCRIPTION in the ending line" $ do
      description `shouldBe` "description"
