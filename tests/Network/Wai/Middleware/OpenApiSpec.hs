module Network.Wai.Middleware.OpenApiSpec
  ( spec
  ) where

import Prelude

import Data.Aeson (encode)
import Network.Wai
import Network.Wai.Middleware.OpenApi qualified as OpenApi
import Network.Wai.Test hiding (request)
import Test.Hspec
import TestApp

spec :: Spec
spec = do
  let
    app :: Application
    app = OpenApi.validate testOpenApi testApp

  describe "validate requests" $ do
    let request = (setPath defaultRequest "/tests") {requestMethod = "POST"}

    it "responds 400 for invalid request bodies" $ do
      withSession app $ do
        sresponse <-
          srequest $
            SRequest
              { simpleRequest = request
              , simpleRequestBody = "{}"
              }

        assertStatus 400 sresponse

    it "passes through valid bodies" $ do
      withSession app $ do
        sresponse <-
          srequest $
            SRequest
              { simpleRequest = request
              , simpleRequestBody = encode $ NewTest "foo"
              }

        assertStatus 201 sresponse
        assertBody (encode $ OK True) sresponse

  describe "validate responses" $ do
    it "responds 500 for invalid response bodies" $ do
      let request = setPath defaultRequest "/tests/42" -- broken
      withSession app $ do
        sresponse <-
          srequest $
            SRequest
              { simpleRequest = request
              , simpleRequestBody = ""
              }

        assertStatus 500 sresponse

    it "passes through valid bodies" $ do
      let request = setPath defaultRequest "/tests/99"
      withSession app $ do
        sresponse <-
          srequest $
            SRequest
              { simpleRequest = request
              , simpleRequestBody = ""
              }

        assertStatus 200 sresponse
        assertBody (encode $ Test "99") sresponse
