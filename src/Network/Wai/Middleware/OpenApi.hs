module Network.Wai.Middleware.OpenApi
  ( validate
  , RequestErrors (..)
  , ResponseErrors (..)
  , SchemaNotFound (..)
  , ValidationErrors (..)
  , validateRequests
  , defaultOnRequestErrors
  , evaluateOnRequestErrors
  , validateResponses
  , defaultOnResponseErrors
  , evaluateOnResponseErrors
  ) where

import Prelude

import Control.Lens ((^.))
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson (Value, eitherDecode)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Data.List.NonEmpty qualified as NE
import Data.OpenApi (Definitions, OpenApi, Schema)
import Data.OpenApi qualified as OpenApi
import Network.Wai (Middleware, Request, Response)
import Network.Wai qualified as Wai
import Network.Wai.Middleware.OpenApi.PathMap qualified as PathMap
import Network.Wai.Middleware.OpenApi.Schema
import Network.Wai.Middleware.OpenApi.Validate
import Network.Wai.Middleware.OpenApi.ValidationError

-- | Validate using 'defaultOnRequestErrors' and 'defaultOnResponseErrors'
validate :: OpenApi -> Middleware
validate spec =
  validateRequests spec defaultOnRequestErrors
    . validateResponses spec defaultOnResponseErrors

data RequestErrors
  = RequestSchemaNotFound SchemaNotFound
  | RequestIsNotJson BSL.ByteString String
  | RequestInvalid Value ValidationErrors
  deriving stock (Show)

defaultOnRequestErrors :: RequestErrors -> Middleware
defaultOnRequestErrors = \case
  RequestSchemaNotFound {} -> id
  RequestIsNotJson {} -> id
  RequestInvalid _ errs -> \_ _ respond ->
    respond $ clientErrorResponse errs

-- | Run the given action and proceed normally
evaluateOnRequestErrors
  :: (RequestErrors -> IO ()) -> RequestErrors -> Middleware
evaluateOnRequestErrors f errs app request respond = do
  f errs
  app request respond

data ResponseErrors
  = ResponseSchemaNotFound SchemaNotFound
  | ResponseIsNotJson BSL.ByteString String
  | ResponseInvalid Value ValidationErrors
  deriving stock (Show)

defaultOnResponseErrors :: ResponseErrors -> Middleware
defaultOnResponseErrors = \case
  ResponseSchemaNotFound {} -> id
  ResponseIsNotJson {} -> id
  ResponseInvalid _ errs -> \_ _ respond ->
    respond $ serverErrorResponse errs

-- | Run the given action and proceed normally
evaluateOnResponseErrors
  :: (ResponseErrors -> IO ()) -> ResponseErrors -> Middleware
evaluateOnResponseErrors f errs app request respond = do
  f errs
  app request respond

validateRequests :: OpenApi -> (RequestErrors -> Middleware) -> Middleware
validateRequests spec onErrors app request0 respond = do
  result <- runValidateT request0 $ do
    schema <-
      modifyError RequestSchemaNotFound $
        lookupRequestSchema spec pathMap
    bytes <- previewRequestBody
    body <- decodeBody RequestIsNotJson bytes
    validateBody RequestInvalid definitions schema body

  case result of
    (Left errs, request1) -> onErrors errs app request1 respond
    (Right (), request1) -> app request1 respond
 where
  pathMap = PathMap.fromOpenApi spec
  definitions = spec ^. OpenApi.components . OpenApi.schemas

validateResponses :: OpenApi -> (ResponseErrors -> Middleware) -> Middleware
validateResponses spec onErrors app request0 respond = do
  app request0 $ \response -> do
    result <- runValidateT request0 $ do
      let status = Wai.responseStatus response
      schema <-
        modifyError ResponseSchemaNotFound $
          lookupResponseSchema status spec pathMap
      bytes <- getResponseBody response
      body <- decodeBody ResponseIsNotJson bytes
      validateBody ResponseInvalid definitions schema body

    case result of
      (Left errs, request1) -> onErrors errs app request1 respond
      (Right (), _) -> respond response
 where
  pathMap = PathMap.fromOpenApi spec
  definitions = spec ^. OpenApi.components . OpenApi.schemas

decodeBody
  :: MonadError e m
  => (BSL.ByteString -> String -> e)
  -> BSL.ByteString
  -> m Value
decodeBody toError bytes =
  either (throwError . toError bytes) pure $ eitherDecode bytes

validateBody
  :: MonadError e m
  => (Value -> ValidationErrors -> e)
  -> Definitions Schema
  -> Schema
  -> Value
  -> m ()
validateBody toError definitions schema body =
  maybe (pure ()) (throwError . toError body . ValidationErrors)
    . NE.nonEmpty
    $ OpenApi.validateJSON definitions schema body

-- | Strictly consume the request body, then mark it as un-consumed
--
-- <https://hackage.haskell.org/package/wai-middleware-validation-0.1.0.2/docs/src/Network.Wai.Middleware.Validation.html#getRequestBody>
previewRequestBody :: (MonadIO m, MonadState Request m) => m BSL.ByteString
previewRequestBody = do
  request <- get
  body <- liftIO $ Wai.strictRequestBody request
  ref <- liftIO $ newIORef body

  -- Update request to mark body as un-consumed
  let newRequestBody = atomicModifyIORef ref (BSL.empty,)
  put $ Wai.setRequestBodyChunks (BSL.toStrict <$> newRequestBody) request

  pure body

getResponseBody :: MonadIO m => Response -> m BSL.ByteString
getResponseBody response = liftIO $ withBody $ \streamingBody -> do
  ref <- newIORef mempty
  streamingBody
    (\b -> atomicModifyIORef ref $ \acc -> (acc <> b, ()))
    (pure ())
  toLazyByteString <$> readIORef ref
 where
  (_, _, withBody) = Wai.responseToStream response
