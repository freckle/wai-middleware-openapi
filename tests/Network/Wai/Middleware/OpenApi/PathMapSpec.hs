module Network.Wai.Middleware.OpenApi.PathMapSpec
  ( spec
  ) where

import Prelude

import Control.Arrow ((&&&))
import Control.Lens ((&), (.~), (?~))
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.OpenApi (OpenApi, PathItem)
import Data.OpenApi qualified as OpenApi
import Data.Text (pack)
import Network.Wai.Middleware.OpenApi.PathMap qualified as PathMap
import Test.Hspec

spec :: Spec
spec = do
  describe "lookup" $ do
    specify "OpenApi precedence example" $ do
      let pathMap =
            PathMap.fromOpenApi $
              testOpenApi
                [ "/pets/{petId}"
                , "/pets/mine"
                ]

      PathMap.lookup "/pets/42" pathMap
        `shouldBe` Just (testPathItem "/pets/{petId}")

      PathMap.lookup "/pets/mine" pathMap
        `shouldBe` Just (testPathItem "/pets/mine")

    specify "Chris' example" $ do
      let pathMap =
            PathMap.fromOpenApi $
              testOpenApi
                [ "/book/{bookId}/cover"
                , "/book/{bookId}/text"
                , "/{resource}/{resourceId}/text"
                , "/{resource}/{resourceId}/ratings"
                ]

      PathMap.lookup "/book/5/text" pathMap
        `shouldBe` Just (testPathItem "/book/{bookId}/text")

      PathMap.lookup "/article/5/text" pathMap
        `shouldBe` Just (testPathItem "/{resource}/{resourceId}/text")

      -- Chris says s/b Nothing
      -- Spec says s/b (4)
      -- Naive implementation, apparently, agrees with spec
      PathMap.lookup "/book/5/ratings" pathMap
        `shouldBe` Just (testPathItem "/{resource}/{resourceId}/ratings")

    -- These tests are to show what happens, but the spec says its ambiguous so
    -- if we do something to the implementation that breaks these, that's OK.
    context "ambiguous according to spec" $ do
      specify "books" $ do
        let pathMap =
              PathMap.fromOpenApi $
                testOpenApi
                  [ "/{entity}/me"
                  , "/books/{id}"
                  ]

        PathMap.lookup "/books/5" pathMap
          `shouldBe` Just (testPathItem "/books/{id}")

        PathMap.lookup "/books/me" pathMap
          `shouldBe` Just (testPathItem "/{entity}/me")

      specify "pet-stores" $ do
        let pathMap =
              PathMap.fromOpenApi $
                testOpenApi
                  [ "/pet-stores/{petStoreId}/pets/mine"
                  , "/pet-stores/{petStoreId}/pets/{petId}"
                  ]

        PathMap.lookup "/pet-stores/5/pets/3" pathMap
          `shouldBe` Just (testPathItem "/pet-stores/{petStoreId}/pets/{petId}")

        PathMap.lookup "/pet-stores/5/pets/mine" pathMap
          `shouldBe` Nothing
          `shouldBe` Just (testPathItem "/pet-stores/{petStoreId}/pets/mine")

testOpenApi :: [String] -> OpenApi
testOpenApi ps = mempty & OpenApi.paths .~ IOHM.fromList (map (id &&& testPathItem) ps)

testPathItem :: String -> PathItem
testPathItem p = mempty & OpenApi.summary ?~ pack p
