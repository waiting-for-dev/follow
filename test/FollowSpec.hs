{-# LANGUAGE OverloadedStrings #-}

module FollowSpec where

import           Control.Monad.Except (runExceptT)
import           Data.Either          (fromRight)
import           Data.Text            (Text)
import qualified Data.Text            as T (concat)
import           Follow
import           Follow.Types         (Digester, Directory (..), Fetcher,
                                       Middleware, MiddlewareArguments (..),
                                       Recipe (..), Result (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".process" $ do
    it "fetches, applies middlewares and digests using given strategies" $ do
      let recipe = Recipe "1.0" "Title" "Description" ["tag"] []
      let fetcher = (\_recipe -> return []) :: Fetcher
      let middleware =
            (\_arg directory ->
               directory {dRecipe = recipe {rTitle = "Title updated"}}) :: Middleware
      let digester =
            (\directory ->
               case dEntries directory of
                 [] ->
                   T.concat [rTitle $ dRecipe directory, " // Empty Entries"]
                 _ -> "Full Entries") :: Digester Text
      result <-
        runExceptT
          (runResult $
           process
             fetcher
             [(middleware, MiddlewareArgumentsDebugging)]
             digester
             recipe)
      fromRight "" result `shouldBe` "Title updated // Empty Entries"
