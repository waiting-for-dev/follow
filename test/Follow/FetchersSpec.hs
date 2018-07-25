module Follow.FetchersSpec where

import           Follow.Fetchers
import           Follow.Types    (Directory (..), Recipe (..), URL (..), URLS)
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetch" $ do
    it "populates Directory URL out of given fetcher" $ do
      let urls =
            [ URL
                "http://url.com"
                "123"
                (Just "Title")
                (Just "Description")
                Nothing
            ]
      let fetcher = \_recipe -> return urls :: IO URLS
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      dURLS <$> directory `shouldReturn` urls
    it "associates given recipe with the Directory" $ do
      let fetcher = \_recipe -> return [] :: IO URLS
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      rTitle <$> (dRecipe <$> directory) `shouldReturn` "Title"
