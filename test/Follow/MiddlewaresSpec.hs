{-# LANGUAGE OverloadedStrings #-}

module Follow.MiddlewaresSpec where

import           Follow.Middlewares
import           Follow.Types       (Directory (..), Entry (..), Middleware,
                                     Recipe (..))
import           Test.Hspec

spec :: Spec
spec =
  describe ".applyMiddlewares" $ do
    let buildEntry title =
          Entry Nothing (Just title) (Just title) Nothing Nothing
    let addEntryMiddleware entry =
          (\directory ->
             Directory (dRecipe directory) $ entry : (dEntries directory)) :: Middleware
    it "applies in order given middlewares" $ do
      let recipe = Recipe "1.0" "Title" "Desc" ["tag"] []
      let directory = Directory recipe []
      let middleware1 = addEntryMiddleware $ buildEntry "A"
      let middleware2 = addEntryMiddleware $ buildEntry "B"
      let result = applyMiddlewares [middleware1, middleware2] directory
      let resultEntries = dEntries result
      eTitle <$> resultEntries `shouldBe` [Just "B", Just "A"]
