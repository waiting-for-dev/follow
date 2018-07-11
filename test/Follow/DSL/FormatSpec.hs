module Follow.DSL.FormatSpec where

import           Data.Either       (isLeft, isRight)
import           Follow.DSL.Format
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe ".nameFormat" $ do
    it "parses given string" $ do
      let input = "name"
      parse (nameFormat "name") "test" input `shouldBe` Right "name"
  describe ".innerLineFormat" $ do
    it "defines a name/value pair and returns the value" $ do
      let input = "FOO bar\n"
      let format = innerLineFormat "FOO" (string "bar")
      parse format "test" input `shouldBe` Right "bar"
    it "accepts leading spaces" $ do
      let input = " \t FOO bar\n"
      let format = innerLineFormat "FOO" (string "bar")
      parse format "test" input `shouldBe` Right "bar"
    it "accepts spaces between name/value pairs" $ do
      let input = "FOO \t bar\n"
      let format = innerLineFormat "FOO" (string "bar")
      parse format "test" input `shouldBe` Right "bar"
    it "accepts trailing spaces" $ do
      let input = "FOO bar \t \n"
      let format = innerLineFormat "FOO" (string "bar")
      parse format "test" input `shouldBe` Right "bar"
    it "requires a new-line char at the end" $ do
      let input = "FOO bar"
      let format = innerLineFormat "FOO" (string "bar")
      let error = parse format "test" input
      error `shouldSatisfy` isLeft
  describe ".endingLineFormat" $ do
    it "does not require a new-line char at the end" $ do
      let input = "FOO bar"
      let format = endingLineFormat "FOO" (string "bar")
      let success = parse format "test" input
      success `shouldSatisfy` isRight
  describe ".multiWordFormat" $ do
    it "allows a single word" $ do
      let input = "foo"
      parse multiWordFormat "test" input `shouldBe` Right "foo"
    it "collapses spaces between words to a single one" $ do
      let input = "foo \t bar"
      parse multiWordFormat "test" input `shouldBe` Right "foo bar"
  describe ".versionLineFormat" $ do
    it "expects inner line with VERSION name" $ do
      let input = "VERSION 1.0\n"
      parse versionLineFormat "test" input `shouldBe` Right "1.0"
  describe ".versionFormat" $ do
    it "expects major and minor numbers separated by a dot" $ do
      let input = "1.2"
      parse versionFormat "test" input `shouldBe` Right "1.2"
  describe ".titleLineFormat" $ do
    it "expects inner line with TITLE name" $ do
      let input = "TITLE Some Title\n"
      parse titleLineFormat "test" input `shouldBe` Right "Some Title"
  describe ".titleFormat" $ do
    it "is multiWordFormat" $ do
      let input = "Some Title"
      parse titleFormat "test" input `shouldBe` Right "Some Title"
  describe ".descriptionLineFormat" $ do
    it "expects inner line with DESCRIPTION name" $ do
      let input = "DESCRIPTION Some Description\n"
      parse descriptionLineFormat "test" input `shouldBe`
        Right "Some Description"
  describe ".descriptionFormat" $ do
    it "is multiWordFormat" $ do
      let input = "Some Description"
      parse descriptionFormat "test" input `shouldBe` Right "Some Description"
  describe ".tagsLineFormat" $ do
    it "expects ending line with TAGS name" $ do
      let input = "TAGS a, b"
      parse tagsLineFormat "test" input `shouldBe` Right ["a", "b"]
  describe ".tagsFormat" $ do
    it "expects a comma separated list of string" $ do
      let input = "foo, bar"
      parse tagsFormat "test" input `shouldBe` Right ["foo", "bar"]
    it "accepts a single tag" $ do
      let input = "foo"
      parse tagsFormat "test" input `shouldBe` Right ["foo"]
    it "removes leading and trailing spaces" $ do
      let input = "foo,   \t  bar  \t  , zoo"
      parse tagsFormat "test" input `shouldBe` Right ["foo", "bar", "zoo"]
    it "collapses spaces between words to a single space" $ do
      let input = "foo \t bar"
      parse tagsFormat "test" input `shouldBe` Right ["foo bar"]
  describe ".format" $ do
    it "returns extracted values" $ do
      let input =
            "VERSION 1.0\n\
             \TITLE foo\n\
             \DESCRIPTION description\n\
             \TAGS taga, tagb"
      let Right (version, title, description, tags) = parse format "test" input
      version `shouldBe` "1.0"
      title `shouldBe` "foo"
      description `shouldBe` "description"
      tags `shouldBe` ["taga", "tagb"]
