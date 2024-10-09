module Network.Wai.Middleware.OpenApi.ValidationError
  ( ValidationErrors (..)
  , ValidationError
  , clientErrorResponse
  , serverErrorResponse
  ) where

import Prelude

import Data.Aeson (ToJSON, Value, encode)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi.Schema.Validation (ValidationError)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (Status, status400, status500)
import Network.Wai (Request, Response, responseLBS)

data ValidationErrors = ValidationErrors
  { request :: Request
  , body :: Value
  , errors :: NonEmpty ValidationError
  }

data Error = Error
  { message :: Text
  , details :: NonEmpty Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

clientErrorResponse :: ValidationErrors -> Response
clientErrorResponse errs =
  errorResponse status400 "Bad Request" $ pack <$> errs.errors

serverErrorResponse :: ValidationErrors -> Response
serverErrorResponse errs =
  errorResponse status500 "Internal Server Error" $ pack <$> errs.errors

errorResponse :: Status -> Text -> NonEmpty Text -> Response
errorResponse status msg =
  responseLBS status [(hContentType, "application/json")]
    . encode
    . Error msg
