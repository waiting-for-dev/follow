{-# LANGUAGE OverloadedStrings #-}

module Follow.DigestersSpec where

import           Data.Text        (Text)
import           Follow.Digesters
import           Follow.Types     (Digester, Directory (..), Header (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".digest" $ do
    it "transform the directory using given digester" $ do
      let digester = (\_directory -> "DIGESTED") :: Digester Text
      let header = Header "Title" "Desc" ["tag"] []
      let directory = Directory header []
      let digested = digest digester directory
      digested `shouldBe` "DIGESTED"
