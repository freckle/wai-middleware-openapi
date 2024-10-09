{-# LANGUAGE ImpredicativeTypes #-}

module Network.Wai.Middleware.OpenApi.Schema
  ( lookupRequestSchema
  , lookupResponseSchema
  ) where

import Prelude

import Control.Lens (ix, to, (^?), _Just)
import Data.OpenApi (OpenApi, Operation, PathItem, Schema)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Schema.Generator qualified as OpenApi (dereference)
import Network.HTTP.Types.Method (StdMethod (..), parseMethod)
import Network.HTTP.Types.Status (Status, statusCode)
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Network.Wai.Middleware.OpenApi.PathMap (PathMap)
import Network.Wai.Middleware.OpenApi.PathMap qualified as PathMap

lookupRequestSchema :: OpenApi -> PathMap -> Request -> Maybe Schema
lookupRequestSchema spec pathMap request = do
  pathItem <- PathMap.lookup (Wai.rawPathInfo request) pathMap
  operation <- getOperation request pathItem
  ref <- operation ^? OpenApi.requestBody . _Just

  definitions <- spec ^? OpenApi.components . OpenApi.requestBodies
  schemas <- spec ^? OpenApi.components . OpenApi.schemas
  OpenApi.dereference definitions ref
    ^? OpenApi.content
      . ix "application/json"
      . OpenApi.schema
      . _Just
      . to (OpenApi.dereference schemas)

lookupResponseSchema
  :: Status -> OpenApi -> PathMap -> Request -> Maybe Schema
lookupResponseSchema status spec pathMap request = do
  pathItem <- PathMap.lookup (Wai.rawPathInfo request) pathMap
  operation <- getOperation request pathItem
  responses <- operation ^? OpenApi.responses
  ref <- case responses ^? ix (statusCode status) of
    Just rr -> Just rr
    Nothing -> responses ^? OpenApi.default_ . _Just

  definitions <- spec ^? OpenApi.components . OpenApi.responses
  schemas <- spec ^? OpenApi.components . OpenApi.schemas
  OpenApi.dereference definitions ref
    ^? OpenApi.content
      . ix "application/json"
      . OpenApi.schema
      . _Just
      . to (OpenApi.dereference schemas)

getOperation :: Request -> PathItem -> Maybe Operation
getOperation request pathItem =
  case parseMethod $ Wai.requestMethod request of
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
