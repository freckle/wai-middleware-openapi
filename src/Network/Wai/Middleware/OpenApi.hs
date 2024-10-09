module Network.Wai.Middleware.OpenApi
  ( Settings (..)
  , defaultSettings
  , ValidationErrors (..)
  , ValidationError
  , validateRequestBody
  , validateResponseBody
  ) where

import Prelude

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Network.Wai (Middleware, Request, Response)
import Network.Wai qualified as Wai
import Network.Wai.Middleware.OpenApi.Schema
import Network.Wai.Middleware.OpenApi.Settings
import Network.Wai.Middleware.OpenApi.Validate
import Network.Wai.Middleware.OpenApi.ValidationError

validateRequestBody :: Settings -> Middleware
validateRequestBody settings app request respond =
  validate
    ValidateOptions
      { settings
      , request
      , schema = lookupRequestSchema settings.spec settings.pathMap request
      , getBody = getRequestBody request
      , respondActual = \case
          Nothing -> app request respond
          Just newReq -> app newReq respond
      , respondErrors = respond . clientErrorResponse
      }

validateResponseBody :: Settings -> Middleware
validateResponseBody settings app request respond =
  app request $ \response -> do
    let status = Wai.responseStatus response
    validate
      ValidateOptions
        { settings
        , request
        , schema = lookupResponseSchema status settings.spec settings.pathMap request
        , getBody = (,()) <$> getResponseBody response
        , respondActual = \_ -> respond response
        , respondErrors = respond . serverErrorResponse
        }

-- | Strictly consume the request body, then mark it as un-consumed
--
-- <https://hackage.haskell.org/package/wai-middleware-validation-0.1.0.2/docs/src/Network.Wai.Middleware.Validation.html#getRequestBody>
getRequestBody :: Request -> IO (BSL.ByteString, Request)
getRequestBody request = do
  body <- Wai.strictRequestBody request
  ref <- newIORef body
  let
    newRequestBody = atomicModifyIORef ref (BSL.empty,)
    newReq = Wai.setRequestBodyChunks (BSL.toStrict <$> newRequestBody) request
  pure (body, newReq)

-- <https://hackage.haskell.org/package/wai-middleware-validation-0.1.0.2/docs/src/Network.Wai.Middleware.Validation.html#getResponseBody>
getResponseBody :: Response -> IO BSL.ByteString
getResponseBody response = withBody $ \streamingBody -> do
  ref <- newIORef mempty
  streamingBody
    (\b -> atomicModifyIORef ref $ \acc -> (acc <> b, ()))
    (pure ())
  toLazyByteString <$> readIORef ref
 where
  (_, _, withBody) = Wai.responseToStream response
