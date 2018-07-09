module Follow.DSL.FormatSpec where

import           Data.List         (intersperse)
import           Follow.DSL.Format
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe "format" $ do
    let successInput = ["VERSION 1.0", "TITLE foo", "DESCRIPTION description"]
    let version = "1.0"
    let title = "foo"
    let description = "description"
    it "defines VERSION in the first line" $ do
      let input = unlines successInput
      let (Right (pVersion, _, _)) = parse format "test" input
      pVersion `shouldBe` version
    it "defines TITLE in the second line" $ do
      let input = unlines successInput
      let (Right (_, pTitle, _)) = parse format "test" input
      pTitle `shouldBe` title
    it "defines DESCRIPTION in the ending line" $ do
      let input = unlines successInput
      let (Right (_, _, pDescription)) = parse format "test" input
      pDescription `shouldBe` description
    it "accepts leading spaces" $ do
      let input = unlines $ map (" \t " ++) successInput
      parse format "test" input `shouldBe` Right (version, title, description)
    it "accepts spaces between name/value pairs" $ do
      let input =
            unlines $ map (unwords . intersperse " \t " . words) successInput
      parse format "test" input `shouldBe` Right (version, title, description)
    it "accepts trailing spaces" $ do
      let input = unlines $ map (++ " \t ") successInput
      parse format "test" input `shouldBe` Right (version, title, description)
    it "does not require newline and the end of the input" $ do
      let input = foldl (++) "" $ intersperse "\n" successInput
      parse format "test" input `shouldBe` Right (version, title, description)
