module Follow.FetchersSpec where

import           Follow.Fetchers
import           Follow.Types    (Directory (..), Entries, Entry (..),
                                  Recipe (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe ".fetch" $ do
    it "populates Directory Entries out of given fetcher" $ do
      let entries =
            [ Entry
                "http://url.com"
                "123"
                (Just "Title")
                (Just "Description")
                Nothing
            ]
      let fetcher = \_recipe -> return entries :: IO Entries
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      dEntries <$> directory `shouldReturn` entries
    it "associates given recipe with the Directory" $ do
      let fetcher = \_recipe -> return [] :: IO Entries
      let recipe = Recipe "1.0" "Title" "Description" ["tag_a"] []
      let directory = fetch recipe fetcher
      rTitle <$> (dRecipe <$> directory) `shouldReturn` "Title"
