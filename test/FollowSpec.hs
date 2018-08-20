{-# LANGUAGE OverloadedStrings #-}

module FollowSpec where

import           Control.Monad.Except (runExceptT)
import           Data.Either          (fromRight)
import           Data.Text            (Text)
import           Follow
import           Follow.Types         (Digester, Directory (..), Fetcher,
                                       Recipe (..), Result (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".process" $ do
    it "fetches and digests using given strategies" $ do
      let recipe = Recipe "1.0" "Title" "Description" ["tag"] []
      let fetcher = (\_recipe -> return []) :: Fetcher
      let digester =
            (\directory ->
               case dEntries directory of
                 [] -> "Empty"
                 _  -> "Full") :: Digester Text
      result <- runExceptT (runResult $ process fetcher digester recipe)
      fromRight "" result `shouldBe` "Empty"
