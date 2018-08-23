{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Follow.DSL.Format.InternalSpec where

import           Data.Dynamic               (fromDynamic, toDyn)
import           Data.Either                (isLeft, isRight)
import           Data.Maybe                 (fromJust)
import           Follow.DSL.Format
import           Follow.DSL.Format.Internal
import           Helpers.Parsec
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe ".lineFormat" $ do
    it "expects first word to be equal to given name" $ do
      let input = "NAME value"
      let ok = parse' (lineFormat "NAME" (string "value")) input
      let ko = parse' (lineFormat "ERROR" (string "value")) input
      ok `shouldSatisfy` isRight
      ko `shouldSatisfy` isLeft
    it "expects format to match after first word" $ do
      let input = "NAME the value"
      let ok = parse' (lineFormat "NAME" (string "the value")) input
      let ko = parse' (lineFormat "NAME" (string "error")) input
      ok `shouldSatisfy` isRight
      ko `shouldSatisfy` isLeft
    it "ignores leading spaces before the name" $ do
      let input = " \t NAME value"
      let result = parse' (lineFormat "NAME" (string "value")) input
      result `shouldSatisfy` isRight
    it "ignores trailing spaces after the value" $ do
      let input = "NAME value \t "
      let result = parse' (lineFormat "NAME" (string "value")) input
      result `shouldSatisfy` isRight
    it "ignores spaces between the name and the value" $ do
      let input = "NAME \t value"
      let result = parse' (lineFormat "NAME" (string "value")) input
      result `shouldSatisfy` isRight
    it "returns what is matched by the format" $ do
      let input = "NAME the value"
      let result = parse' (lineFormat "NAME" (string "the value")) input
      result `shouldBe` Right "the value"
  describe ".nameFormat" $ do
    it "matches given string" $ do
      let input = "NAME"
      let ok = parse' (nameFormat "NAME") input
      let ko = parse' (nameFormat "ERROR") input
      ok `shouldSatisfy` isRight
      ko `shouldSatisfy` isLeft
    it "requires name to be uppercased in the source" $ do
      let inputKo = "name"
      let inputOk = "NAME"
      let ko = parse' (nameFormat "name") inputKo
      let ok = parse' (nameFormat "name") inputOk
      ko `shouldSatisfy` isLeft
      ok `shouldSatisfy` isRight
    it "returns unit element" $ do
      let input = "NAME"
      let result = parse' (nameFormat "NAME") input
      result `shouldBe` Right ()
  describe ".versionFormat" $ do
    it "accepts 1.0" $ do
      let input = "1.0"
      let result = parse' versionFormat input
      result `shouldSatisfy` isRight
    it "does not accept anything else" $ do
      let input = "1.1"
      let result = parse' versionFormat input
      result `shouldSatisfy` isLeft
    it "returns what is matched" $ do
      let input = "1.0"
      let result = parse' versionFormat input
      result `shouldBe` Right "1.0"
  describe ".versionLineFormat" $
    it "is a line with VERSION and .versionFormat" $ do
      let input = "VERSION 1.0"
      let result = parse' versionLineFormat input
      result `shouldBe` Right "1.0"
  describe ".titleFormat" $
    it "is multiWordFormat" $ do
      let input = "Some, title!"
      let result = parse' titleFormat input
      result `shouldBe` Right "Some, title!"
  describe ".titleLineFormat" $
    it "is a line with TITLE and .titleFormat" $ do
      let input = "TITLE Some Title"
      let result = parse' titleLineFormat input
      result `shouldBe` Right "Some Title"
  describe ".descriptionFormat" $
    it "is multiWordFormat" $ do
      let input = "This is some great, great description!"
      let result = parse' descriptionFormat input
      result `shouldBe` Right "This is some great, great description!"
  describe ".descriptionLineFormat" $
    it "is a line with DESCRIPTION and .descriptionFormat" $ do
      let input = "DESCRIPTION Some Description\n"
      let result = parse' descriptionLineFormat input
      result `shouldBe` Right "Some Description"
  describe ".tagFormat" $ do
    it "matches letters" $ do
      let input = "ABC"
      let result = parse' tagFormat input
      result `shouldSatisfy` isRight
    it "matches digits" $ do
      let input = "123"
      let result = parse' tagFormat input
      result `shouldSatisfy` isRight
    it "matches underscores" $ do
      let input = "_"
      let result = parse' tagFormat input
      result `shouldSatisfy` isRight
    it "matches hyphens" $ do
      let input = "-"
      let result = parse' tagFormat input
      result `shouldSatisfy` isRight
    it "returns what is matched" $ do
      let input = "A1_-"
      let result = parse' tagFormat input
      result `shouldBe` Right "A1_-"
    it "collapses spaces between words to a single one" $ do
      let input = "my \t tag"
      let result = parse' tagFormat input
      result `shouldBe` Right "my tag"
  describe ".tagsFormat" $
    it "is .csFormat with .tagFormat" $ do
      let input = "my \t tag_1, tag-2"
      let result = parse' tagsFormat input
      result `shouldBe` Right ["my tag_1", "tag-2"]
  describe ".tagsLineFormat" $
    it "is a line with TAGS and .tagsFormat" $ do
      let input = "TAGS a, b"
      let result = parse' tagsLineFormat input
      result `shouldBe` Right ["a", "b"]
  describe ".argumentsFormat" $ do
    it "matches argumentDSL names as line names" $ do
      let input =
            "\n\
                  \NAME1 value1\n\
                  \NAME2 value2"
      let argumentsDSLOk =
            [ ("NAME1", toDyn <$> string "value1")
            , ("NAME2", toDyn <$> string "value2")
            ]
      let argumentsDSLKo =
            [ ("NAME1", toDyn <$> string "value1")
            , ("ERROR", toDyn <$> string "value2")
            ]
      let ok = parse' (argumentsFormat argumentsDSLOk) input
      let ko = parse' (argumentsFormat argumentsDSLKo) input
      ok `shouldSatisfy` isRight
      ko `shouldSatisfy` isLeft
    it "matches using argumentDSL value formats" $ do
      let input =
            "NAME1 value1\n\
             \NAME2 value2"
      let argumentsDSL =
            [ ("NAME1", toDyn <$> string "value1")
            , ("NAME2", toDyn <$> string "value2")
            ]
      let result = parse' (argumentsFormat argumentsDSL) input
      result `shouldSatisfy` isLeft
    it "requires a newline character at the begining of the input" $ do
      let input =
            "\n\
                  \NAME1 value1\n\
                  \NAME2 value2"
      let argumentsDSLOk =
            [ ("NAME1", toDyn <$> string "value1")
            , ("NAME2", toDyn <$> string "value2")
            ]
      let argumentsDSLKo =
            [ ("NAME1", toDyn <$> string "error")
            , ("ERROR", toDyn <$> string "value2")
            ]
      let ok = parse' (argumentsFormat argumentsDSLOk) input
      let ko = parse' (argumentsFormat argumentsDSLKo) input
      ok `shouldSatisfy` isRight
      ko `shouldSatisfy` isLeft
    it "returns list of tuples with name and what is matched by value formats" $ do
      let input =
            "\n\
                  \NAME1 value1\n\
                  \NAME2 value2"
      let argumentsDSL =
            [ ("NAME1", toDyn <$> string "value1")
            , ("NAME2", toDyn <$> string "value2")
            ]
      let result = parse' (argumentsFormat argumentsDSL) input
      let result' =
            fmap (\(name, value) -> (name, fromDynamic value :: Maybe String)) <$>
            result
      result' `shouldBe`
        Right [("NAME1", Just "value1"), ("NAME2", Just "value2")]
  describe ".format" $ do
    it "returns a tuple with all extracted values from lines" $ do
      let input =
            "VERSION 1.0\n\
             \TITLE title\n\
             \DESCRIPTION description\n\
             \TAGS tag_a, tag_b\n\
             \ARG1 value1\n\
             \ARG2 value2"
      let argumentsDSL =
            [ ("ARG1", toDyn <$> string "value1")
            , ("ARG2", toDyn <$> string "value2")
            ]
      let result = parse' (format argumentsDSL) input
      let result' =
            fmap
              (\(t, d, tg, args) ->
                 ( t
                 , d
                 , tg
                 , map (\(n, v) -> (n, fromDynamic v :: Maybe String)) args))
              result
      result' `shouldBe`
        Right
          ( "title"
          , "description"
          , ["tag_a", "tag_b"]
          , [("ARG1", Just "value1"), ("ARG2", Just "value2")])
    it "allows a line break at the end" $ do
      let input =
            "VERSION 1.0\n\
             \TITLE title\n\
             \DESCRIPTION description\n\
             \TAGS tag_a, tag_b\n\
             \ARG1 value1\n"
      let argumentsDSL = [("ARG1", toDyn <$> string "value1")]
      let result = parse' (format argumentsDSL) input
      result `shouldSatisfy` isRight
