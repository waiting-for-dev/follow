module Follow.DSL.ParserSpec where

import           Data.Dynamic (fromDynamic, toDyn)
import           Data.Maybe   (fromJust)
import           Follow       (Recipe (..))
import           Follow.DSL
import           Paths_follow (getDataFileName)
import           Test.Hspec
import           Text.Parsec  (string)

spec :: Spec
spec = do
  let argumentsDSL =
        [ ("ARG1", toDyn <$> string "value_1")
        , ("ARG2", toDyn <$> string "value_2")
        ]
  let argumentsFromDyn =
        fmap (\(n, v) -> (n, fromJust (fromDynamic v :: Maybe String)))
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
      let recipe = parseDSL input argumentsDSL
      rVersion <$> recipe `shouldBe` Right "1.0"
      rTitle <$> recipe `shouldBe` Right "title"
      rDescription <$> recipe `shouldBe` Right "description"
      rTags <$> recipe `shouldBe` Right ["tag_a", "tag_b"]
      argumentsFromDyn <$>
        (rArguments <$> recipe) `shouldBe`
        Right [("ARG1", "value_1"), ("ARG2", "value_2")]
  describe ".parseDSLFile" $
    it "parses DSL file to a recipe" $ do
      path <- getDataFileName "test/Fixtures/Recipe"
      recipe <- parseDSLFile path argumentsDSL
      rVersion <$> recipe `shouldBe` Right "1.0"
      rTitle <$> recipe `shouldBe` Right "title"
      rDescription <$> recipe `shouldBe` Right "description"
      rTags <$> recipe `shouldBe` Right ["tag_a", "tag_b"]
      argumentsFromDyn <$>
        (rArguments <$> recipe) `shouldBe`
        Right [("ARG1", "value_1"), ("ARG2", "value_2")]
