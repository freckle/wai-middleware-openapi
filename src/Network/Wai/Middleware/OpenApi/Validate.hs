-- | Shared validation routine between request and response bodies
module Network.Wai.Middleware.OpenApi.Validate
  ( ValidateOptions (..)
  , validate
  ) where

import Prelude

import Control.Lens ((^.))
import Data.Aeson (Value, decode)
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty qualified as NE
import Data.OpenApi qualified as OpenApi
import Network.Wai (Request)
import Network.Wai.Middleware.OpenApi.Settings
import Network.Wai.Middleware.OpenApi.ValidationError

data ValidateOptions extra a = ValidateOptions
  { settings :: Settings
  , request :: Request
  , schema :: Maybe OpenApi.Schema
  , getBody :: IO (BSL.ByteString, extra)
  , respondActual :: Maybe extra -> IO a
  , respondErrors :: ValidationErrors -> IO a
  }

-- | Internal DRY'ing of request-vs-response validation
validate :: ValidateOptions extra a -> IO a
validate options = do
  case options.schema of
    Nothing -> options.respondActual Nothing
    Just schema -> do
      (bytes, extra) <- options.getBody

      let
        defns = options.settings.spec ^. OpenApi.components . OpenApi.schemas
        merrs = do
          body <- decode @Value bytes
          errors <- NE.nonEmpty $ OpenApi.validateJSON defns schema body
          pure ValidationErrors {request = options.request, body, errors}

      case merrs of
        Nothing -> options.respondActual $ Just extra
        Just errs -> do
          options.settings.onValidationErrors errs

          if options.settings.evaluateOnly
            then options.respondActual $ Just extra
            else options.respondErrors errs
