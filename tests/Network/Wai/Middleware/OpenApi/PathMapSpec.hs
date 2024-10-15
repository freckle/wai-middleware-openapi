module Network.Wai.Middleware.OpenApi.PathMapSpec
  ( spec
  ) where

import Prelude

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import Data.String (IsString)
import Network.Wai.Middleware.OpenApi.PathMap qualified as PathMap
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "PathMap" $ do
    it "empty matches nothing" $
      property $
        \path ->
          isNothing $ PathMap.lookup @Bool (renderPath path) PathMap.empty

    it "case 1" $
      property $
        \segment ->
          PathMap.lookup
            (renderPath Path {segments = [segment]})
            (PathMap.fromList [("/{}", True)])
            == Just True

    it "case 2" $
      property $
        forAll (arbitrary `suchThat` (/= "a")) $ \segment ->
          let result =
                PathMap.lookup
                  (renderPath $ Path [segment])
                  (PathMap.fromList [("/a", False), ("/{}", True)])
          in  counterexample ("result = " <> show result) (result == Just True)

    it "case 3" $
      property $
        forAll (arbitrary `suchThat` (/= "a")) $ \segment ->
          let result =
                PathMap.lookup
                  (renderPath $ Path [segment])
                  (PathMap.fromList [("/{}", False), ("/a", True)])
          in  counterexample ("result = " <> show result) (isNothing result)

-- Failed attempt at writing a property test because I cannot find
-- any sensible property that 'lookup' satisfies:

-- it "returns Just the rightmost match ???" $
--   property $
--     \path ->
--       forAll
--         ( do
--             whatever <- fmap (,False) <$> listOf (genPattern path)
--             accepter <- (,True) <$> genAccepter path
--             rejecters <- fmap (,False) <$> listOf (genRejecter path)
--             pure $
--               zipWith (\i (pat, ok) -> (pat, (i, ok))) [1 :: Word ..] $
--                 whatever <> [accepter] <> rejecters
--         )
--         $ \patterns ->
--           case PathMap.lookup
--             (renderPath path)
--             (PathMap.fromList $ first renderPattern <$> patterns) of
--             Nothing -> counterexample "No match" $ property False
--             Just (i, ok) -> counterexample ("Matched pattern " <> show i) $ property ok

newtype PathSegment = PathSegment {string :: FilePath}
  deriving stock (Eq)
  deriving newtype (Show, IsString)

instance Arbitrary PathSegment where
  arbitrary = fmap PathSegment $ vectorOf 3 $ choose ('a', 'z')

newtype Path = Path {segments :: [PathSegment]}
  deriving stock (Eq)
  deriving newtype (Arbitrary, Semigroup, Monoid)

instance Show Path where
  show = foldMap (("/" <>) . (.string)) . (.segments)

newtype Pattern = Pattern {segments :: [Maybe PathSegment]}
  deriving stock (Eq)
  deriving newtype (Arbitrary, Semigroup, Monoid)

instance Show Pattern where
  show = foldMap (("/" <>) . maybe "{}" (.string)) . (.segments)

renderPath :: Path -> ByteString
renderPath = BS8.pack . show

renderPattern :: Pattern -> FilePath
renderPattern = show

-- | Generate a 'Pattern' based on a given 'Path'
-- --
-- The 'Pattern' may or may not match the 'Path', but has a
-- nontrivial probability of accepting it.
genPattern :: Path -> Gen Pattern
genPattern path = oneof [genAccepter path, genRejecter path]

-- | Generate a 'Pattern' that accepts the given 'Path'
genAccepter :: Path -> Gen Pattern
genAccepter Path {segments} =
  Pattern <$> traverse (\x -> elements [Just x, Nothing]) segments

-- | Generate a 'Pattern' than rejects the given 'Path'
--
-- There is a nontrivial probability that the 'Pattern' will
-- exhibit various similarities to the 'Path'.
genRejecter :: Path -> Gen Pattern
genRejecter path =
  oneof $
    toList (genShortRejecter path)
      <> toList (genEquilengthRejecter path)
      <> [genLongRejecter path]

-- | Generate a 'Pattern' that does not accept the given 'Path'
--   because the 'Pattern' is longer than the 'Path'
--
-- There is a nontrivial probability that the first /n/ segments
-- of the 'Pattern' will match a 'Path' with /n/ segments.
genLongRejecter :: Path -> Gen Pattern
genLongRejecter path = do
  enough <- oneof [genAccepter path, genEquilengthPattern path]
  more <- Pattern <$> ((:) <$> arbitrary <*> arbitrary)
  pure $ enough <> more

-- | Generate a 'Pattern' that does not accept the given 'Path'
--   because the 'Pattern' is shorter than the 'Path'
--
-- There is a nontrivial probability that some segments of the
-- 'Pattern' will match those of the 'Path'.
genShortRejecter :: Path -> Maybe (Gen Pattern)
genShortRejecter Path {segments} = do
  guard $ not $ null segments
  Just $ sized $ \n -> do
    l <- choose (0, max 1 n) `suchThat` (/= length segments)
    Pattern <$> vectorOf l arbitrary

-- | Generate a 'Pattern' that has the same number of segments
--   as the given 'Path'
--
-- The 'Pattern' may or may not match the 'Path', but has a
-- nontrivial probability of accepting it.
genEquilengthPattern :: Path -> Gen Pattern
genEquilengthPattern Path {segments} =
  Pattern
    <$> traverse (\x -> oneof [pure (Just x), arbitrary]) segments

-- | Generate a 'Pattern' that has the same length as the given
--   'Path' but does not accept it
--
-- There is a nontrivial probability that some segments of the
-- 'Pattern' will match those of the 'Path'.
genEquilengthRejecter :: Path -> Maybe (Gen Pattern)
genEquilengthRejecter Path {segments} = do
  guard $ not $ null segments
  Just $ do
    -- The index at which we'll definitely not match
    i <- choose (0, length segments - 1)
    fmap Pattern
      <$> traverse
        ( \(i', x) ->
            if i == i'
              then Just <$> arbitrary `suchThat` (/= x)
              else oneof [pure (Just x), arbitrary]
        )
      $ zip [0 ..] segments
