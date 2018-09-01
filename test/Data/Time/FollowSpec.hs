{-# LANGUAGE OverloadedStrings #-}

module Data.Time.FollowSpec where

import           Data.Time        (defaultTimeLocale, formatTime,
                                   iso8601DateFormat)
import           Data.Time.Follow
import           Test.Hspec

spec :: Spec
spec =
  describe ".parseTimeGuess" $ do
    it "parses from iso8601" $ do
      let string = "2018-09-02"
      let time = parseTimeGuess string
      (show <$> time) `shouldBe` Just "2018-09-02 00:00:00 UTC"
    it "parses from iso8601 with time" $ do
      let string = "2018-09-02T01:00:00"
      let time = parseTimeGuess string
      (show <$> time) `shouldBe` Just "2018-09-02 01:00:00 UTC"
    it "parses from iso8601 with time and timezone" $ do
      let string = "2018-09-02T01:00:00+0000"
      let time = parseTimeGuess string
      (show <$> time) `shouldBe` Just "2018-09-02 01:00:00 UTC"
    it "parses from iso8601 with fractions of seconds" $ do
      let string = "2018-09-02T01:00:00.001000"
      let time = parseTimeGuess string
      (show <$> time) `shouldBe` Just "2018-09-02 01:00:00.001 UTC"
    it "parses from iso8601 with fractions of seconds and timezone" $ do
      let string = "2018-09-02T01:00:00.001000+0000"
      let time = parseTimeGuess string
      (show <$> time) `shouldBe` Just "2018-09-02 01:00:00.001 UTC"
    it "parses from rfc822" $ do
      let string = "Sun, 02 Sep 2018 01:00:00 +0000"
      let time = parseTimeGuess string
      (show <$> time) `shouldBe` Just "2018-09-02 01:00:00 UTC"
    it "parses from epoch" $ do
      let string = "1535846400"
      let time = parseTimeGuess string
      (show <$> time) `shouldBe` Just "2018-09-02 00:00:00 UTC"
    it "returns Nothing when unknown" $ do
      let string = "None"
      let time = parseTimeGuess string
      time `shouldBe` Nothing
