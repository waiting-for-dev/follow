{-# LANGUAGE OverloadedStrings #-}

module FollowSpec where

import           Control.Monad.Except (runExceptT)
import           Data.Either          (fromRight)
import           Data.Text            (Text)
import qualified Data.Text            as T (concat)
import           Follow
import           Follow.Types         (Digester, Directory (..), Fetched,
                                       Header (..), Middleware, Result (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".process" $ do
    it "fetches, applies middlewares and digests using given strategies" $ do
      let header = Header "1.0" "Title" "Description" ["tag"] []
      let fetched = return [] :: Fetched
      let middleware =
            (\directory ->
               directory {dHeader = header {hTitle = "Title updated"}}) :: Middleware
      let digester =
            (\directory ->
               case dEntries directory of
                 [] ->
                   T.concat [hTitle $ dHeader directory, " // Empty Entries"]
                 _ -> "Full Entries") :: Digester Text
      result <-
        runExceptT (runResult $ process fetched [middleware] digester header)
      fromRight "" result `shouldBe` "Title updated // Empty Entries"
