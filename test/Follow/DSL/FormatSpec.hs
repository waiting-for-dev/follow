module Follow.DSL.FormatSpec where

import           Data.Either            (isLeft, isRight)
import           Follow.DSL.Format
import           Follow.Strategies.Null as Null (argumentsDSL)
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe ".nameFormat" $ do
    it "parses given string" $ do
      let input = "name"
      parse (nameFormat "name") "test" input `shouldBe` Right "name"
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
      let input = "Some, title!"
      parse titleFormat "test" input `shouldBe` Right "Some, title!"
  describe ".descriptionLineFormat" $ do
    it "expects inner line with DESCRIPTION name" $ do
      let input = "DESCRIPTION Some Description\n"
      parse descriptionLineFormat "test" input `shouldBe`
        Right "Some Description"
  describe ".descriptionFormat" $ do
    it "is multiWordFormat" $ do
      let input = "This is some great, great description!"
      parse descriptionFormat "test" input `shouldBe`
        Right "This is some great, great description!"
  describe ".tagsLineFormat" $ do
    it "expects inner line with TAGS name" $ do
      let input = "TAGS a, b\n"
      parse tagsLineFormat "test" input `shouldBe` Right ["a", "b"]
  describe ".tagFormat" $ do
    it "allows digits, alphas, _ and -" $ do
      let input = "1A_-"
      parse tagFormat "test" input `shouldBe` Right "1A_-"
  describe ".tagsFormat" $ do
    it "is cswFormat" $ do
      let input = "foo \t bar, zoo_1"
      parse tagsFormat "test" input `shouldBe` Right ["foo bar", "zoo_1"]
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
  describe ".format" $ do
    it "returns extracted values" $ do
      let input =
            "VERSION 1.0\n\
             \TITLE foo\n\
             \DESCRIPTION description\n\
             \TAGS tag_a, tag_b\n"
      let Right (version, title, description, tags, _arguments) =
            parse (format Null.argumentsDSL) "test" input
      version `shouldBe` "1.0"
      title `shouldBe` "foo"
      description `shouldBe` "description"
      tags `shouldBe` ["tag_a", "tag_b"]
      let hardInput =
            "       VERSION \t 1.0  \t \n\
             \  \t TITLE   Great, title!    \n\
             \   DESCRIPTION    This is some great, great description!\n\
             \TAGS   \t  tag_a, tag-b"
      let Right (version, title, description, tags, _arguments) =
            parse (format Null.argumentsDSL) "test" hardInput
      version `shouldBe` "1.0"
      title `shouldBe` "Great, title!"
      description `shouldBe` "This is some great, great description!"
      tags `shouldBe` ["tag_a", "tag-b"]
