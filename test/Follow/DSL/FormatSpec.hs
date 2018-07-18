module Follow.DSL.FormatSpec where

import           Follow.DSL.Format
import           Follow.DSL.Format.Internal
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe ".lineFormat" $ do
    it "defines a name/value pair and returns the value" $ do
      let input = "FOO bar\n"
      let format = lineFormat "FOO" (string "bar")
      parse format "test" input `shouldBe` Right "bar"
    it "accepts leading spaces" $ do
      let input = " \t FOO bar\n"
      let format = lineFormat "FOO" (string "bar")
      parse format "test" input `shouldBe` Right "bar"
    it "accepts spaces between name/value pairs" $ do
      let input = "FOO \t bar\n"
      let format = lineFormat "FOO" (string "bar")
      parse format "test" input `shouldBe` Right "bar"
    it "accepts trailing spaces" $ do
      let input = "FOO bar \t \n"
      let format = lineFormat "FOO" (string "bar")
      parse format "test" input `shouldBe` Right "bar"
  describe ".wordFormat" $ do
    it "allows digits, alphas, puntuation and symbols" $ do
      let input = "1A,+"
      parse wordFormat "test" input `shouldBe` Right "1A,+"
  describe ".multiWordFormat" $ do
    it "allows a single word" $ do
      let input = "foo,+"
      parse multiWordFormat "test" input `shouldBe` Right "foo,+"
    it "collapses spaces between words to a single one" $ do
      let input = "foo \t bar"
      parse multiWordFormat "test" input `shouldBe` Right "foo bar"
  describe ".csFormat" $ do
    let format = csFormat (many1 alphaNum)
    it "expects a comma separated list of items" $ do
      let input = "foo, bar"
      parse format "test" input `shouldBe` Right ["foo", "bar"]
    it "accepts a single word" $ do
      let input = "foo"
      parse format "test" input `shouldBe` Right ["foo"]
    it "removes leading and trailing spaces" $ do
      let input = "foo,   \t  bar  \t  , zoo"
      parse format "test" input `shouldBe` Right ["foo", "bar", "zoo"]
    it "collapses spaces between words to a single space" $ do
      let input = "foo \t bar"
      parse format "test" input `shouldBe` Right ["foo bar"]
