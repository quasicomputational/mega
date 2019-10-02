module Main
  ( main
  )
  where

import qualified Control.Lens as Lens
import Control.Lens.Properties ( isIso )
import Data.Int ( Int8 )
import Data.Map ( Map )
import GHC.Generics ( Generic )
import Test.Tasty ( testGroup, defaultMain )
--Note: only import the specifically tasty-related things from tasty-quickcheck, because of https://github.com/feuerbach/tasty/issues/208
import Test.Tasty.QuickCheck ( testProperty )
import qualified Test.QuickCheck as QC

import Q4C12.ComposeKey
  ( composeKey, decomposeKey )

newtype NonEmptyMap k v = NonEmptyMap { getNonEmptyMap :: Map k v }
  deriving ( Generic, Show, Eq )

instance ( QC.Arbitrary k, Ord k ) => QC.Arbitrary1 ( NonEmptyMap k ) where
  liftArbitrary v = fmap NonEmptyMap $ ( QC.liftArbitrary v ) `QC.suchThat` (\ m -> length m > 0 )
  liftShrink v =
    fmap NonEmptyMap . filter (\ shrunk -> length shrunk > 0 ) . QC.liftShrink v . getNonEmptyMap

instance ( QC.Arbitrary k, Ord k, QC.Arbitrary v ) => QC.Arbitrary ( NonEmptyMap k v ) where
  arbitrary = QC.liftArbitrary QC.arbitrary
  shrink = QC.liftShrink pure

instance ( QC.CoArbitrary k, QC.CoArbitrary v ) => QC.CoArbitrary ( NonEmptyMap k v )

instance ( QC.Function k, Ord k, QC.Function v ) => QC.Function ( NonEmptyMap k v )

composed :: Lens.Iso' ( Map ( Int8, Int ) Integer ) ( Map Int8 ( NonEmptyMap Int Integer ) )
composed = Lens.iso ( fmap NonEmptyMap . decomposeKey ) ( composeKey . fmap getNonEmptyMap )

main :: IO ()
main = defaultMain $ testGroup "property tests"
  [ testProperty "is an Iso" $ isIso composed
  ]
