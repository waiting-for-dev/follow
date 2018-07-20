module Follow.DSL.ParserSpec where

import           Data.Dynamic (fromDynamic, toDyn)
import           Follow.DSL
import           Test.Hspec
import           Text.Parsec  (string)

spec :: Spec
spec = do
  describe ".parseDSL" $
    it "parses DSL to a recipe" $ do
      let input =
            unlines
              [ "VERSION 1.0"
              , "TITLE title"
              , "DESCRIPTION description"
              , "TAGS tag_a, tag_b"
              , "ARG1 value_1"
              , "ARG2 value_2"
              ]
      let argumentsDSL =
            [ ("ARG1", toDyn <$> string "value_1")
            , ("ARG2", toDyn <$> string "value_2")
            ]
      let recipe = parseDSL input argumentsDSL
      rVersion <$> recipe `shouldBe` Right "1.0"
      rTitle <$> recipe `shouldBe` Right "title"
      rDescription <$> recipe `shouldBe` Right "description"
      rTags <$> recipe `shouldBe` Right ["tag_a", "tag_b"]
      fmap (\(n, v) -> (n, fromDynamic v :: Maybe String)) <$>
        (rStrategyArguments <$> recipe) `shouldBe`
        Right [("ARG1", Just "value_1"), ("ARG2", Just "value_2")]
