{-# LANGUAGE OverloadedStrings #-}

module Follow.DigestersSpec where

import           Data.Text        (Text)
import           Follow.Digesters
import           Follow.Types     (Digester, Directory (..), Subject (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".digest" $ do
    it "transform the directory using given digester" $ do
      let digester = (\_directory -> "DIGESTED") :: Digester Text
      let subject = Subject "Title" "Desc" ["tag"]
      let directory = Directory subject []
      let digested = digest digester directory
      digested `shouldBe` "DIGESTED"
