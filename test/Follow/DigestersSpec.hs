{-# LANGUAGE OverloadedStrings #-}

module Follow.DigestersSpec where

import           Data.Text        (Text)
import           Follow.Digesters
import           Follow.Types     (Digester, Directory (..), Recipe (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".digest" $ do
    it "transform the directory using given digester" $ do
      let digester = (\_directory -> "DIGESTED") :: Digester Text
      let recipe = Recipe "1.0" "Title" "Desc" ["tag"] []
      let directory = Directory recipe []
      let digested = digest digester directory
      digested `shouldBe` "DIGESTED"
