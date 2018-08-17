{-# LANGUAGE OverloadedStrings #-}

module Follow.DSL.FormatSpec where

import           Data.Either       (isLeft, isRight)
import           Follow.DSL.Format
import           Helpers.Parsec
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe ".wordFormat" $ do
    it "allows letters on it" $ do
      let input = "ABC"
      let result = parse' wordFormat input
      result `shouldSatisfy` isRight
    it "allows digits on it" $ do
      let input = "123"
      let result = parse' wordFormat input
      result `shouldSatisfy` isRight
    it "allows puntuations on it" $ do
      let input = ",."
      let result = parse' wordFormat input
      result `shouldSatisfy` isRight
    it "allows symbols on it" $ do
      let input = "+"
      let result = parse' wordFormat input
      result `shouldSatisfy` isRight
    it "returns what is matched" $ do
      let input = "A1,+"
      let result = parse' wordFormat input
      result `shouldBe` Right "A1,+"
  describe ".multiWordFormat" $ do
    it "returns multiple words (.wordFormat) separated by spaces" $ do
      let input = "word1+ word2!"
      let result = parse' multiWordFormat input
      result `shouldBe` Right "word1+ word2!"
    it "allows a single word" $ do
      let input = "word"
      let result = parse' multiWordFormat input
      result `shouldBe` Right "word"
    it "collapses spaces between words to a single one" $ do
      let input = "word1 \t word2"
      let result = parse' multiWordFormat input
      result `shouldBe` Right "word1 word2"
  describe ".csFormat" $ do
    it "returns a list of items of given format that were separated by a comma" $ do
      let input = "value1, value2"
      let result = parse' (csFormat $ many1 alphaNum) input
      result `shouldBe` Right ["value1", "value2"]
    it "accepts a single item" $ do
      let input = "value"
      let result = parse' (csFormat $ string "value") input
      result `shouldBe` Right ["value"]
    it "removes leading spaces in items" $ do
      let input = "value1,   \t  value2, value3"
      let result = parse' (csFormat $ many1 alphaNum) input
      result `shouldBe` Right ["value1", "value2", "value3"]
    it "removes trailing spaces in items" $ do
      let input = "value1,value2 \t , value3"
      let result = parse' (csFormat $ many1 alphaNum) input
      result `shouldBe` Right ["value1", "value2", "value3"]
  describe ".spaceFormat" $ do
    it "matches spaces" $ do
      let input = "  "
      let result = parse' spaceFormat input
      result `shouldSatisfy` isRight
    it "matches tabs" $ do
      let input = "\t\t"
      let result = parse' spaceFormat input
      result `shouldSatisfy` isRight
    it "allows a single space" $ do
      let input = " "
      let result = parse' spaceFormat input
      result `shouldSatisfy` isRight
    it "requires at least one space" $ do
      let input = "a"
      let result = parse' spaceFormat input
      result `shouldSatisfy` isLeft
    it "returns unit element" $ do
      let input = " "
      let result = parse' spaceFormat input
      result `shouldBe` Right ()
  describe ".optionalSpaceFormat" $ do
    it "matches spaces (.spaceFormat)" $ do
      let input = "\t "
      let result = parse' optionalSpaceFormat input
      result `shouldSatisfy` isRight
    it "allows no space" $ do
      let input = "a"
      let result = parse' optionalSpaceFormat input
      result `shouldSatisfy` isRight
    it "returns unit element" $ do
      let input = " "
      let result = parse' optionalSpaceFormat input
      result `shouldBe` Right ()
  describe ".uriFormat" $ do
    it "matches a valid URI" $ do
      let inputOk = "http://anurl.com"
      let inputKo = "error"
      let ok = parse' uriFormat inputOk
      let ko = parse' uriFormat inputKo
      ok `shouldSatisfy` isRight
      ko `shouldSatisfy` isLeft
    it "does not match a relative URI" $ do
      let input = "/anurl.com"
      let result = parse' uriFormat input
      result `shouldSatisfy` isLeft
    it "does not match a URI with fragment identifier" $ do
      let input = "http://anurl.com#here"
      let result = parse' uriFormat input
      result `shouldSatisfy` isLeft
    it "allows a percent encoded URI" $ do
      let input = "http://anurl%21.com"
      let result = parse' uriFormat input
      result `shouldSatisfy` isRight
    it "returns matched URI as string" $ do
      let input = "http://anurl.com"
      let result = parse' uriFormat input
      result `shouldBe` Right "http://anurl.com"
