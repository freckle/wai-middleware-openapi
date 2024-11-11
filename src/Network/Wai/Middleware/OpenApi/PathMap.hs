module Network.Wai.Middleware.OpenApi.PathMap
  ( PathMap
  , fromOpenApi
  , lookup
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Lens ((^?))
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Function (on)
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.List (find)
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.OpenApi (OpenApi, PathItem)
import Data.OpenApi qualified as OpenApi
import System.FilePath.Posix qualified as Posix

newtype PathMap = PathMap
  { unwrap :: [(TemplatedPath, PathItem)]
  }

fromOpenApi :: OpenApi -> PathMap
fromOpenApi spec =
  PathMap $ case spec ^? OpenApi.paths of
    Nothing -> []
    Just ps -> map (first toTemplatedPath) $ IOHM.toList ps

lookup :: ByteString -> PathMap -> Maybe PathItem
lookup bs pm =
  fmap snd $ find (matchExact tp . fst) ps <|> find (matchTemplated tp . fst) ps
 where
  tp = toTemplatedPath $ BS8.unpack bs
  ps = pm.unwrap

matchExact :: TemplatedPath -> TemplatedPath -> Bool
matchExact = matchComponents (==) `on` (.unwrap)

matchTemplated :: TemplatedPath -> TemplatedPath -> Bool
matchTemplated = matchComponents go `on` (.unwrap)
 where
  go = curry $ \case
    (Exact l, Exact r) -> l == r
    (ParameterValue, _) -> True
    (_, ParameterValue) -> True

newtype TemplatedPath = TemplatedPath
  { unwrap :: [TemplatedPathComponent]
  }
  deriving stock (Eq)

toTemplatedPath :: FilePath -> TemplatedPath
toTemplatedPath =
  TemplatedPath
    . map toTemplatedPathComponent
    . Posix.splitDirectories

data TemplatedPathComponent
  = Exact FilePath
  | ParameterValue
  deriving stock (Eq)

toTemplatedPathComponent :: FilePath -> TemplatedPathComponent
toTemplatedPathComponent s = fromMaybe (Exact s) $ do
  ne <- nonEmpty s
  guard $ NE.head ne == '{'
  guard $ NE.last ne == '}'
  pure ParameterValue

matchComponents
  :: (TemplatedPathComponent -> TemplatedPathComponent -> Bool)
  -> [TemplatedPathComponent]
  -> [TemplatedPathComponent]
  -> Bool
matchComponents f as bs
  | length as /= length bs = False
  | otherwise = and $ zipWith f as bs
