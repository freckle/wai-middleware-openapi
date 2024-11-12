module Network.Wai.Middleware.OpenApi.Validate
  ( ValidateT (..)
  , runValidateT
  ) where

import Prelude

import Control.Monad.Except
import Control.Monad.State
import Network.Wai (Request)

newtype ValidateT e m a = ValidateT
  { unwrap :: ExceptT e (StateT Request m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError e
    , MonadState Request
    )

runValidateT
  :: Request
  -> ValidateT e m a
  -> m (Either e a, Request)
runValidateT request f = runStateT (runExceptT f.unwrap) request
