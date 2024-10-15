module Network.Wai.Middleware.OpenApi.PathMap (
  PathMap,
  fromOpenApi,
  fromList,
  lookup,
  empty,
) where

import Prelude hiding (lookup)

import Control.Lens ((^?))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.HashMap.Strict.InsOrd qualified as IHashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.OpenApi (OpenApi, PathItem)
import Data.OpenApi qualified as OpenApi
import System.FilePath.Posix qualified as Posix

newtype PathMap a = PathMap
  { unwrap :: Map TemplatedPath a
  }

empty :: PathMap a
empty = PathMap Map.empty

fromOpenApi :: OpenApi -> PathMap PathItem
fromOpenApi spec =
  maybe empty (fromList . IHashMap.toList) $ spec ^? OpenApi.paths

fromList :: [(FilePath, a)] -> PathMap a
fromList = PathMap . Map.fromList . map (first toTemplatedPath)

lookup :: ByteString -> PathMap a -> Maybe a
lookup p pm = Map.lookup (toTemplatedPath $ BS8.unpack p) pm.unwrap

newtype TemplatedPath = TemplatedPath
  { _unwrap :: [TemplatedPathComponent]
  }
  deriving stock (Eq, Ord)

toTemplatedPath :: FilePath -> TemplatedPath
toTemplatedPath =
  TemplatedPath
    . map toTemplatedPathComponent
    . Posix.splitDirectories

data TemplatedPathComponent
  = Exact FilePath
  | ParameterValue

instance Eq TemplatedPathComponent where
  Exact l == Exact r = l == r
  _ == _ = True

instance Ord TemplatedPathComponent where
  compare (Exact l) (Exact r) = compare l r
  compare _ _ = EQ

toTemplatedPathComponent :: FilePath -> TemplatedPathComponent
toTemplatedPathComponent s
  | not (null s) && head s == '{' && last s == '}' = ParameterValue
  | otherwise = Exact s
