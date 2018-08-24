{-# LANGUAGE OverloadedStrings #-}

module Follow.DSL.ParserSpec where

import           Data.Dynamic (fromDynamic, toDyn)
import           Data.Maybe   (fromJust)
import           Follow.DSL
import           Follow.Types (Subject (..))
import           Paths_follow (getDataFileName)
import           Test.Hspec
import           Text.Parsec  (string)

spec :: Spec
spec = do
  let argumentsDSL =
        [ ("ARG1", toDyn <$> string "value_1")
        , ("ARG2", toDyn <$> string "value_2")
        ]
  describe ".parseDSL" $
    it "parses DSL to a subject" $ do
      let input =
            unlines
              [ "VERSION 1.0"
              , "TITLE title"
              , "DESCRIPTION description"
              , "TAGS tag_a, tag_b"
              , "ARG1 value_1"
              , "ARG2 value_2"
              ]
      let subject = parseDSL input argumentsDSL
      sTitle <$> subject `shouldBe` Right "title"
      sDescription <$> subject `shouldBe` Right "description"
      sTags <$> subject `shouldBe` Right ["tag_a", "tag_b"]
  describe ".parseDSLFile" $
    it "parses DSL file to a subject" $ do
      path <- getDataFileName "test/Fixtures/Subject"
      subject <- parseDSLFile path argumentsDSL
      sTitle <$> subject `shouldBe` Right "title"
      sDescription <$> subject `shouldBe` Right "description"
      sTags <$> subject `shouldBe` Right ["tag_a", "tag_b"]
