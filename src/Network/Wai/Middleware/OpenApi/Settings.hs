module Network.Wai.Middleware.OpenApi.Settings
  ( Settings (..)
  , defaultSettings
  ) where

import Prelude

import Data.OpenApi (OpenApi)
import Network.Wai.Middleware.OpenApi.PathMap (PathMap)
import Network.Wai.Middleware.OpenApi.PathMap qualified as PathMap
import Network.Wai.Middleware.OpenApi.ValidationError

data Settings = Settings
  { spec :: OpenApi
  , pathMap :: PathMap
  , onValidationErrors :: ValidationErrors -> IO ()
  , evaluateOnly :: Bool
  }

defaultSettings :: OpenApi -> Settings
defaultSettings spec =
  Settings
    { spec
    , pathMap = PathMap.fromOpenApi spec
    , onValidationErrors = \_ -> pure ()
    , evaluateOnly = False
    }
