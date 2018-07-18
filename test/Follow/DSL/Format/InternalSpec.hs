module Follow.DSL.Format.InternalSpec where

import           Data.Dynamic               (fromDynamic, toDyn)
import           Data.Either                (isLeft, isRight)
import           Data.Maybe                 (fromJust)
import           Follow.DSL.Format
import           Follow.DSL.Format.Internal
import           Follow.Strategies.Null     as Null (argumentsDSL)
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe ".nameFormat" $
    it "parses given string" $ do
      let input = "name"
      parse (nameFormat "name") "test" input `shouldBe` Right "name"
  describe ".versionLineFormat" $
    it "expects line with VERSION name" $ do
      let input = "VERSION 1.0\n"
      parse versionLineFormat "test" input `shouldBe` Right "1.0"
  describe ".versionFormat" $
    it "expects major and minor numbers separated by a dot" $ do
      let input = "1.2"
      parse versionFormat "test" input `shouldBe` Right "1.2"
  describe ".titleLineFormat" $
    it "expects line with TITLE name" $ do
      let input = "TITLE Some Title\n"
      parse titleLineFormat "test" input `shouldBe` Right "Some Title"
  describe ".titleFormat" $
    it "is multiWordFormat" $ do
      let input = "Some, title!"
      parse titleFormat "test" input `shouldBe` Right "Some, title!"
  describe ".descriptionLineFormat" $
    it "expects line with DESCRIPTION name" $ do
      let input = "DESCRIPTION Some Description\n"
      parse descriptionLineFormat "test" input `shouldBe`
        Right "Some Description"
  describe ".descriptionFormat" $
    it "is multiWordFormat" $ do
      let input = "This is some great, great description!"
      parse descriptionFormat "test" input `shouldBe`
        Right "This is some great, great description!"
  describe ".tagsLineFormat" $
    it "expects line with TAGS name" $ do
      let input = "TAGS a, b\n"
      parse tagsLineFormat "test" input `shouldBe` Right ["a", "b"]
  describe ".tagFormat" $
    it "allows digits, alphas, _ and -" $ do
      let input = "1A_-"
      parse tagFormat "test" input `shouldBe` Right "1A_-"
  describe ".tagsFormat" $
    it "is cswFormat" $ do
      let input = "foo \t bar, zoo_1"
      parse tagsFormat "test" input `shouldBe` Right ["foo bar", "zoo_1"]
  describe ".argumentsFormat" $
    it "extracts according given arguments DSL" $ do
      let input = "\nARG1 VAL1\nARG2 VAL2"
      let argumentsDSL =
            [("ARG1", toDyn <$> wordFormat), ("ARG2", toDyn <$> wordFormat)]
      let (Right parsed) = parse (argumentsFormat argumentsDSL) "test" input
      let parsedConcrete =
            map
              (\(name, value) ->
                 (name, fromJust (fromDynamic value :: Maybe String)))
              parsed
      parsedConcrete `shouldBe` [("ARG1", "VAL1"), ("ARG2", "VAL2")]
  describe ".format" $
    it "returns extracted values" $ do
      let input =
            "VERSION 1.0\n\
             \TITLE foo\n\
             \DESCRIPTION description\n\
             \TAGS tag_a, tag_b\n"
      let Right (version, title, description, tags, arguments) =
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
