module TestApp
  ( NewTest (..)
  , Test (..)
  , OK (..)
  , testApp
  , testOpenApi
  ) where

import Prelude

import Control.Lens (at, (&), (.~), (?~))
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.OpenApi (OpenApi, ToSchema, declareSchemaRef)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Declare (evalDeclare, looks)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status
  ( Status
  , status200
  , status201
  , status404
  , status405
  )
import Network.Wai

newtype NewTest = NewTest
  { name :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype Test = Test
  { name :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype OK = OK
  { ok :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

testApp :: Application
testApp request respond =
  respond $ dispatch (pathInfo request) (requestMethod request)

dispatch :: [Text] -> Method -> Response
dispatch = \case
  ["tests"] -> dispatchTests
  ["tests", x] -> dispatchTest x
  _ -> const response404

dispatchTests :: Method -> Response
dispatchTests = \case
  "POST" -> responseJSON status201 $ OK True
  _ -> response405

dispatchTest :: Text -> Method -> Response
dispatchTest name = \case
  "GET" -> case name of
    "42" -> responseLBS status200 [] "{}" -- invalid
    _ -> responseJSON status200 $ Test name
  _ -> response405

responseJSON :: ToJSON a => Status -> a -> Response
responseJSON status =
  responseLBS status [(hContentType, "application/json")] . encode

response404 :: Response
response404 = responseLBS status404 [] "Not Found"

response405 :: Response
response405 = responseLBS status405 [] "Bad Method"

testOpenApi :: OpenApi
testOpenApi = flip evalDeclare mempty $ do
  newTestRef <- declareSchemaRef $ Proxy @NewTest
  testRef <- declareSchemaRef $ Proxy @Test
  okRef <- declareSchemaRef $ Proxy @OK

  looks $ \d ->
    mempty
      & OpenApi.components . OpenApi.schemas .~ d
      & OpenApi.components . OpenApi.requestBodies
        .~ IOHM.fromList
          [
            ( "Test"
            , mempty
                & OpenApi.content . at "application/json"
                  ?~ (mempty & OpenApi.schema ?~ newTestRef)
            )
          ]
      & OpenApi.paths
        .~ IOHM.fromList
          [
            ( "/tests"
            , mempty
                & OpenApi.post
                  ?~ ( mempty
                        & OpenApi.requestBody
                          ?~ OpenApi.Ref (OpenApi.Reference "Test")
                        & at 201
                          ?~ ( "OK"
                                & OpenApi._Inline
                                  . OpenApi.content
                                  . at "application/json"
                                  ?~ (mempty & OpenApi.schema ?~ okRef)
                             )
                     )
            )
          ,
            ( "/tests/{name}"
            , mempty
                & OpenApi.get
                  ?~ ( mempty
                        & at 200
                          ?~ ( "OK"
                                & OpenApi._Inline
                                  . OpenApi.content
                                  . at "application/json"
                                  ?~ (mempty & OpenApi.schema ?~ testRef)
                             )
                     )
            )
          ]
