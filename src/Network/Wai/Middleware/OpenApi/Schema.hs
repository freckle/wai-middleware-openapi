module Network.Wai.Middleware.OpenApi.Schema
  ( SchemaNotFound (..)
  , lookupRequestSchema
  , lookupResponseSchema
  ) where

import Prelude

import Control.Applicative (asum)
import Control.Lens (Lens', ix, to, (^?), _Just)
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Maybe (fromMaybe)
import Data.OpenApi
  ( Components
  , Definitions
  , HasContent
  , MediaTypeObject
  , OpenApi
  , Operation
  , PathItem
  , Referenced
  , Schema
  )
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Schema.Generator qualified as OpenApi (dereference)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types.Method (StdMethod (..), parseMethod)
import Network.HTTP.Types.Status (Status, statusCode)
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Network.Wai.Middleware.OpenApi.PathMap (PathMap)
import Network.Wai.Middleware.OpenApi.PathMap qualified as PathMap

data SchemaNotFound
  = -- | The OpenAPI spec doesn't have this path at all
    MissingPath ByteString
  | -- | The specified path does not have this operation (method)
    MissingOperation ByteString
  | -- | The specified operation defines no request body
    MissingRequestBody
  | -- | The specified operation defines no response for this status
    MissingResponseStatus Status
  | -- | The specified body or response defines no JSON content
    --
    -- We are only able to validate JSON schema. If you are configuring things
    -- such that 'SchemaNotFound' are errors, you likely still want to avoid
    -- erroring on this one.
    MissingContentTypeJson
  deriving stock (Show)

lookupRequestSchema
  :: (MonadError SchemaNotFound m, MonadState Request m)
  => OpenApi
  -> PathMap
  -> m Schema
lookupRequestSchema spec =
  lookupOperationSchema spec $ \operation -> do
    note MissingRequestBody $
      operation
        ^? OpenApi.requestBody
          . _Just
          . to (dereference spec OpenApi.requestBodies)

lookupResponseSchema
  :: (MonadError SchemaNotFound m, MonadState Request m)
  => Status
  -> OpenApi
  -> PathMap
  -> m Schema
lookupResponseSchema status spec =
  lookupOperationSchema spec $ \operation -> do
    ref <- note (MissingResponseStatus status) $ do
      responses <- operation ^? OpenApi.responses
      asum
        [ responses ^? ix (statusCode status)
        , responses ^? OpenApi.default_ . _Just
        ]
    pure $ dereference spec OpenApi.responses ref

lookupOperationSchema
  :: ( MonadState Request m
     , MonadError SchemaNotFound m
     , HasContent body (InsOrdHashMap MediaType MediaTypeObject)
     )
  => OpenApi
  -> (Operation -> m body)
  -> PathMap
  -> m Schema
lookupOperationSchema spec getBody pathMap = do
  request <- get
  pathItem <- getPathItem request pathMap
  operation <- getOperation request pathItem
  body <- getBody operation
  note MissingContentTypeJson $
    body
      ^? OpenApi.content
        . ix "application/json"
        . OpenApi.schema
        . _Just
        . to (dereference spec OpenApi.schemas)

getPathItem
  :: MonadError SchemaNotFound m
  => Request
  -> PathMap
  -> m PathItem
getPathItem request = note (MissingPath path) . PathMap.lookup path
 where
  path = Wai.rawPathInfo request

getOperation
  :: MonadError SchemaNotFound m
  => Request
  -> PathItem
  -> m Operation
getOperation request pathItem =
  note (MissingOperation method) $ case parseMethod method of
    Right DELETE -> pathItem ^? OpenApi.delete . _Just
    Right GET -> pathItem ^? OpenApi.get . _Just
    Right PATCH -> pathItem ^? OpenApi.patch . _Just
    Right POST -> pathItem ^? OpenApi.post . _Just
    Right PUT -> pathItem ^? OpenApi.put . _Just
    Right HEAD -> Nothing
    Right TRACE -> Nothing
    Right CONNECT -> Nothing
    Right OPTIONS -> Nothing
    Left _ -> Nothing
 where
  method = Wai.requestMethod request

dereference :: OpenApi -> Lens' Components (Definitions a) -> Referenced a -> a
dereference spec componentL = OpenApi.dereference definitions
 where
  definitions = fromMaybe mempty $ spec ^? OpenApi.components . componentL

note :: MonadError e m => e -> Maybe a -> m a
note e = maybe (throwError e) pure
