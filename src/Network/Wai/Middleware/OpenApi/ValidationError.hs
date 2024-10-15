module Network.Wai.Middleware.OpenApi.ValidationError
  ( ValidationErrors (..)

    -- * JSON error responses
  , Error (..)
  , clientErrorResponse
  , serverErrorResponse
  , errorResponse
  ) where

import Prelude

import Data.Aeson (ToJSON, encode)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi.Schema.Validation qualified as OpenApi
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (Status, status400, status500)
import Network.Wai (Response, responseLBS)

newtype ValidationErrors = ValidationErrors
  { unwrap :: NonEmpty OpenApi.ValidationError
  }
  deriving newtype (Show)

data Error = Error
  { message :: Text
  , details :: NonEmpty Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

clientErrorResponse :: ValidationErrors -> Response
clientErrorResponse errors =
  errorResponse status400 "Bad Request" $ pack <$> errors.unwrap

serverErrorResponse :: ValidationErrors -> Response
serverErrorResponse errors =
  errorResponse status500 "Internal Server Error" $ pack <$> errors.unwrap

errorResponse :: Status -> Text -> NonEmpty Text -> Response
errorResponse status msg =
  responseLBS status [(hContentType, "application/json")]
    . encode
    . Error msg
