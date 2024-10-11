{-# LANGUAGE ImpredicativeTypes #-}

module Network.Wai.Middleware.OpenApi.Schema
  ( SchemaNotFound (..)
  , lookupRequestSchema
  , lookupResponseSchema
  ) where

import Prelude

import Control.Lens (ix, to, (^?), _Just)
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.OpenApi (OpenApi, Operation, PathItem, Schema)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Schema.Generator qualified as OpenApi (dereference)
import Data.String (IsString)
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
  | -- | The specified request does not have a body specified
    MissingRequestBody
  | -- | The specified request does not have this content(-type)
    MissingContent ByteString
  | -- | The specified response does not have this status
    MissingResponseStatus Status
  | -- | The specified response does not have this content(-type)
    MissingResponseContent ByteString
  | -- | The specified content is not JSON (we can't validate it)
    NonJsonRequestBody BSL.ByteString String
  | -- | The specified response content is not JSON (we can't validate it)
    NonJsonResponseBody BSL.ByteString String
  deriving stock (Show)

note :: MonadError e m => e -> Maybe a -> m a
note e = maybe (throwError e) pure

wip :: MonadError SchemaNotFound m => Maybe a -> m a
wip = maybe (throwError $ MissingPath "/todo") pure

lookupRequestSchema
  :: (MonadError SchemaNotFound m, MonadState Request m)
  => OpenApi
  -> PathMap
  -> m Schema
lookupRequestSchema spec pathMap = do
  request <- get
  pathItem <- getPathItem request pathMap
  operation <- getOperation request pathItem
  ref <- note MissingRequestBody $ operation ^? OpenApi.requestBody . _Just
  let body = OpenApi.dereference definitions ref
  ct <-
    note (MissingContent contentTypeJson) $
      body ^? OpenApi.content . ix contentTypeJson . OpenApi.schema . _Just
  pure $ OpenApi.dereference schemas ct
 where
  definitions = fromMaybe mempty $ spec ^? OpenApi.components . OpenApi.requestBodies
  schemas = fromMaybe mempty $ spec ^? OpenApi.components . OpenApi.schemas

lookupResponseSchema
  :: (MonadError SchemaNotFound m, MonadState Request m)
  => Status
  -> OpenApi
  -> PathMap
  -> m Schema
lookupResponseSchema status spec pathMap = do
  request <- get
  pathItem <- getPathItem request pathMap
  operation <- getOperation request pathItem
  responses <- wip $ operation ^? OpenApi.responses
  ref <- wip $ case responses ^? ix (statusCode status) of
    Just rr -> Just rr
    Nothing -> responses ^? OpenApi.default_ . _Just

  let
    definitions = fromMaybe mempty $ spec ^? OpenApi.components . OpenApi.responses
    schemas = fromMaybe mempty $ spec ^? OpenApi.components . OpenApi.schemas

  wip $
    OpenApi.dereference definitions ref
      ^? OpenApi.content
        . ix "application/json"
        . OpenApi.schema
        . _Just
        . to (OpenApi.dereference schemas)

getPathItem
  :: MonadError SchemaNotFound m
  => Request
  -> PathMap
  -> m PathItem
getPathItem request pathMap =
  note (MissingPath path) $ PathMap.lookup path pathMap
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

contentTypeJson :: IsString s => s
contentTypeJson = "application/json"
