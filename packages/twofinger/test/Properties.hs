{-# LANGUAGE TemplateHaskell #-}
--TODO: It's nice being able to edit the properties as code, which we couldn't have if they were haddocks, but it'd also be nice to have some way to cross-link them to the relevant functions in the Q4C12.TwoFinger's generated haddocks; maybe even to generate pretty documentation of the properties?
module Main
  ( main
  )
  where

import Control.Lens (Lens', makePrisms)
import Control.Lens.Properties (isLens)
import Control.Monad (join)
import Data.Semigroup ((<>))
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck (Gen, Arbitrary, Arbitrary1, Arbitrary2, testProperty, shrink2, arbitrary2, liftArbitrary, liftArbitrary2, liftShrink, liftShrink2)
import qualified Test.Tasty.QuickCheck as QC

import Q4C12.TwoFinger.Internal (TwoFingerOddA (TwoFingerOddA), TwoFingerOddE (TwoFingerOddE), TwoFingerEvenA (TwoFingerEvenA), TwoFingerEvenE (EmptyEvenE, TwoFingerEvenE), halfsnocOddA, halfsnocOddE, halfunsnocOddA, halfconsEvenE, halfunconsOddA, halfconsOddE, halfsnocEvenA, appendOddEOddA, appendEvenEOddE, appendOddEEvenA, appendOddAOddE, appendEvenAOddA, appendOddAEvenE, halfunsnocEvenA, halfunsnocEvenE, halfsnocEvenE, halfunconsEvenA, halfunsnocOddE, halfunconsEvenE, halfconsOddA, halfunconsOddE, halfconsEvenA, firstOddA, lastOddA)

newtype AnyOddA e a = AnyOddA { getAnyOddA :: TwoFingerOddA e a }
  deriving (Show, Eq)

instance Arbitrary2 AnyOddA where
  liftArbitrary2 e a = fmap AnyOddA $
    TwoFingerOddA <$> liftArbitrary (liftArbitrary2 a e) <*> a
  liftShrink2 f g (AnyOddA (TwoFingerOddA as a)) = do
    as' <- liftShrink (liftShrink2 g f) as
    a' <- g a
    pure $ AnyOddA $ TwoFingerOddA as' a'

instance (Arbitrary e) => Arbitrary1 (AnyOddA e) where
  liftArbitrary = liftArbitrary2 QC.arbitrary
  liftShrink = liftShrink2 pure

instance (Arbitrary e, Arbitrary a) => Arbitrary (AnyOddA e a) where
  arbitrary = arbitrary2
  shrink = shrink2

newtype AnyOddE e a = AnyOddE { _getAnyOddE :: TwoFingerOddE e a }
  deriving (Show, Eq)

instance Arbitrary2 AnyOddE where
  liftArbitrary2 e a = fmap AnyOddE $ TwoFingerOddE <$> e <*> liftArbitrary (liftArbitrary2 a e)
  liftShrink2 f g (AnyOddE (TwoFingerOddE e as)) = do
    e' <- f e
    as' <- liftShrink (liftShrink2 g f) as
    pure $ AnyOddE $ TwoFingerOddE e' as'

instance (Arbitrary e) => Arbitrary1 (AnyOddE e) where
  liftArbitrary = liftArbitrary2 QC.arbitrary
  liftShrink = liftShrink2 pure

instance (Arbitrary e, Arbitrary a) => Arbitrary (AnyOddE e a) where
  arbitrary = arbitrary2
  shrink = shrink2

newtype AnyEvenA e a = AnyEvenA { _getAnyEvenA :: TwoFingerEvenA e a }
  deriving (Show, Eq)

instance Arbitrary2 AnyEvenA where
  liftArbitrary2 e a = AnyEvenA . TwoFingerEvenA <$> liftArbitrary (liftArbitrary2 a e)
  liftShrink2 f g (AnyEvenA (TwoFingerEvenA as)) =
    AnyEvenA . TwoFingerEvenA <$> liftShrink (liftShrink2 g f) as

instance (Arbitrary e) => Arbitrary1 (AnyEvenA e) where
  liftArbitrary = liftArbitrary2 QC.arbitrary
  liftShrink = liftShrink2 pure

instance (Arbitrary e, Arbitrary a) => Arbitrary (AnyEvenA e a) where
  arbitrary = arbitrary2
  shrink = shrink2

newtype AnyEvenE e a = AnyEvenE { _getAnyEvenE :: TwoFingerEvenE e a }
  deriving (Show, Eq)

instance Arbitrary2 AnyEvenE where
  liftArbitrary2 e a = AnyEvenE <$> QC.oneof
    [ pure EmptyEvenE
    , TwoFingerEvenE <$> e <*> liftArbitrary (liftArbitrary2 a e) <*> a
    ]
  liftShrink2 _ _ (AnyEvenE EmptyEvenE) = []
  liftShrink2 f g (AnyEvenE (TwoFingerEvenE e as a)) = fmap AnyEvenE $ do
    e' <- f e
    as' <- liftShrink (liftShrink2 g f) as
    a' <- g a
    [ mempty, TwoFingerEvenE e' as' a' ]

instance (Arbitrary e) => Arbitrary1 (AnyEvenE e) where
  liftArbitrary = liftArbitrary2 QC.arbitrary
  liftShrink = liftShrink2 pure

instance (Arbitrary e, Arbitrary a) => Arbitrary (AnyEvenE e a) where
  arbitrary = arbitrary2
  shrink = shrink2

makePrisms ''AnyOddA
makePrisms ''AnyOddE
makePrisms ''AnyEvenA
makePrisms ''AnyEvenE

intFields :: (p Int [Int] -> r) -> p Int [Int] -> r
intFields = id

halfconsProperties :: TestTree
halfconsProperties = testGroup "half operations"
  [ testGroup "halfcons"
    [ testProperty "OddA" $ \e -> intFields $ \(AnyOddA as) ->
        halfunconsEvenE (halfconsOddA e as) == Just (e, as)
    , testProperty "OddE" $ \a -> intFields $ \(AnyOddE as) ->
        halfunconsEvenA (halfconsOddE a as) == Just (a, as)
    , testProperty "EvenA" $ \e -> intFields $ \(AnyEvenA as) ->
        halfunconsOddE (halfconsEvenA e as) == (e, as)
    , testProperty "EvenE" $ \a -> intFields $ \(AnyEvenE as) ->
        halfunconsOddA (halfconsEvenE a as) == (a, as)
    ]
  , testGroup "halfsnoc"
    [ testProperty "OddA" $ \e -> intFields $ \(AnyOddA as) ->
        halfunsnocEvenA (halfsnocOddA as e) == Just (as, e)
    , testProperty "OddE" $ \a -> intFields $ \(AnyOddE as) ->
        halfunsnocEvenE (halfsnocOddE as a) == Just (as, a)
    , testProperty "EvenA" $ \a -> intFields $ \(AnyEvenA as) ->
        halfunsnocOddA (halfsnocEvenA as a) == (as, a)
    , testProperty "EvenE" $ \e -> intFields $ \(AnyEvenE as) ->
        halfunsnocOddE (halfsnocEvenE as e) == (as, e)
    ]
  , testGroup "halfuncons"
    [ testProperty "OddA" $ intFields $ \(AnyOddA as) ->
        as == uncurry halfconsEvenE (halfunconsOddA as)
    , testProperty "OddE" $ intFields $  \(AnyOddE as) ->
        as == uncurry halfconsEvenA (halfunconsOddE as)
    , testProperty "EvenA" $ intFields $ \(AnyEvenA as) ->
        as == maybe mempty (uncurry halfconsOddE) (halfunconsEvenA as)
    , testProperty "EvenE" $ intFields $ \(AnyEvenE as) ->
        as == maybe mempty (uncurry halfconsOddA) (halfunconsEvenE as)
    ]
  , testGroup "halfunsnoc"
    [ testProperty "OddA" $ intFields $ \(AnyOddA as) ->
        as == uncurry halfsnocEvenA (halfunsnocOddA as)
    , testProperty "OddE" $ intFields $  \(AnyOddE as) ->
        as == uncurry halfsnocEvenE (halfunsnocOddE as)
    , testProperty "EvenA" $ intFields $ \(AnyEvenA as) ->
        as == maybe mempty (uncurry halfsnocOddA) (halfunsnocEvenA as)
    , testProperty "EvenE" $ intFields $ \(AnyEvenE as) ->
        as == maybe mempty (uncurry halfsnocOddE) (halfunsnocEvenE as)
    ]
  ]

associativeProperties :: TestTree
associativeProperties = testGroup "associativity"
  [ testGroup "leftmost OddA"
    [ testProperty "OddA OddA" $ intFields $ \(AnyOddA a) (AnyOddA b) (AnyOddA c) ->
        a <> (b <> c) == (a <> b) <> c
    , testProperty "OddA EvenE" $ intFields $ \(AnyOddA a) (AnyOddA b) (AnyEvenE c) ->
        appendOddAEvenE (a <> b) c == a <> appendOddAEvenE b c
    , testProperty "OddE OddA" $ intFields $ \(AnyOddA a) (AnyOddE b) (AnyOddA c) ->
        appendEvenAOddA (appendOddAOddE a b) c == appendOddAEvenE a (appendOddEOddA b c)
    , testProperty "OddE EvenA" $ intFields $ \(AnyOddA a) (AnyOddE b) (AnyEvenA c) ->
        appendOddAOddE a (appendOddEEvenA b c) == appendOddAOddE a b <> c
    , testProperty "EvenE OddE" $ intFields $ \(AnyOddA a) (AnyEvenE b) (AnyOddE c) ->
        appendOddAOddE a (appendEvenEOddE b c) == appendOddAOddE (appendOddAEvenE a b) c
    , testProperty "EvenE EvenE" $ intFields $ \(AnyOddA a) (AnyEvenE b) (AnyEvenE c) ->
        appendOddAEvenE a (b <> c) == appendOddAEvenE (appendOddAEvenE a b) c
    ]
  , testGroup "leftmost OddE"
    [ testProperty "OddA OddE" $ intFields $ \(AnyOddE a) (AnyOddA b) (AnyOddE c) ->
        appendOddEEvenA a (appendOddAOddE b c) == appendEvenEOddE (appendOddEOddA a b) c
    , testProperty "OddA EvenE" $ intFields $ \(AnyOddE a) (AnyOddA b) (AnyEvenE c) ->
        appendOddEOddA a (appendOddAEvenE b c) == appendOddEOddA a b <> c
    , testProperty "EvenA OddA" $ intFields $ \(AnyOddE a) (AnyEvenA b) (AnyOddA c) ->
        appendOddEOddA a (appendEvenAOddA b c) == appendOddEOddA (appendOddEEvenA a b) c
    , testProperty "EvenA EvenA" $ intFields $ \(AnyOddE a) (AnyEvenA b) (AnyEvenA c) ->
        appendOddEEvenA a (b <> c) == appendOddEEvenA (appendOddEEvenA a b) c
    ]
  , testGroup "leftmost EvenA"
    [ testProperty "EvenA EvenA" $ intFields $ \(AnyEvenA a) (AnyEvenA b) (AnyEvenA c) ->
        a <> (b <> c) == (a <> b) <> c
    , testProperty "OddA OddA" $ intFields $ \(AnyEvenA a) (AnyOddA b) (AnyOddA c) ->
        appendEvenAOddA a (b <> c) == appendEvenAOddA a b <> c
    , testProperty "OddA OddE" $ intFields $ \(AnyEvenA a) (AnyOddA b) (AnyOddE c) ->
        appendOddAOddE (appendEvenAOddA a b) c == a <> appendOddAOddE b c
    , testProperty "OddA EvenE" $ intFields $ \(AnyEvenA a) (AnyOddA b) (AnyEvenE c) ->
        appendOddAEvenE (appendEvenAOddA a b) c == appendEvenAOddA a (appendOddAEvenE b c)
    , testProperty "EvenA OddA" $ intFields $ \(AnyEvenA a) (AnyEvenA b) (AnyOddA c) ->
        appendEvenAOddA (a <> b) c == appendEvenAOddA a (appendEvenAOddA b c)
    ]
  , testGroup "leftmost EvenE"
    [ testProperty "EvenE EvenE" $ intFields $ \(AnyEvenE a) (AnyEvenE b) (AnyEvenE c) ->
        a <> (b <> c) == (a <> b) <> c
    , testProperty "OddE OddE" $ intFields $ \(AnyEvenE a) (AnyOddE b) (AnyOddA c) ->
        appendOddEOddA (appendEvenEOddE a b) c == a <> appendOddEOddA b c
    , testProperty "OddE EvenA" $ intFields $ \(AnyEvenE a) (AnyOddE b) (AnyEvenA c) ->
        appendOddEEvenA (appendEvenEOddE a b) c == appendEvenEOddE a (appendOddEEvenA b c)
    , testProperty "EvenE OddE" $ intFields $ \(AnyEvenE a) (AnyEvenE b) (AnyOddE c) ->
        appendEvenEOddE (a <> b) c == appendEvenEOddE a (appendEvenEOddE b c)
    ]
  ]

monoidIdentityProperties :: TestTree
monoidIdentityProperties = testGroup "monoidal identity"
  [ testProperty "left OddA OddA" $ intFields $ \(AnyOddA a) ->
      a == mempty <> a
  , testProperty "right OddA OddA" $ intFields $ \(AnyOddA a) ->
      a == a <> mempty
  , testProperty "left EvenA EvenA" $ intFields $ \(AnyEvenA a) ->
      a == mempty <> a
  , testProperty "right EvenA EvenA" $ intFields $ \(AnyEvenA a) ->
      a == a <> mempty
  , testProperty "left EvenE EvenE" $ intFields $ \(AnyEvenE a) ->
      a == mempty <> a
  , testProperty "right EvenE EvenE" $ intFields $ \(AnyEvenE a) ->
      a == a <> mempty
  , testProperty "right OddA EvenE" $ intFields $ \(AnyOddA a) ->
      a == appendOddAEvenE a mempty
  , testProperty "left EvenA OddA" $ intFields $ \(AnyOddA a) ->
      a == appendEvenAOddA mempty a
  , testProperty "left EvenE OddE" $ intFields $ \(AnyOddE a) ->
      a == appendEvenEOddE mempty a
  , testProperty "right OddE EvenA" $ intFields $ \(AnyOddE a) ->
      a == appendOddEEvenA a mempty
  ]

monadProperties :: TestTree
monadProperties = testGroup "OddA monad laws"
  [ testProperty "join . join === join . fmap join" $
      --Cut size, since we generate three layers multiplicatively.
      let gen :: Gen a -> Gen (TwoFingerOddA Int a)
          gen x = QC.scale (`mod` 50) $ getAnyOddA <$> QC.liftArbitrary x
      in QC.forAll (gen $ gen $ gen QC.arbitrary) $ \as ->
           join (join as) == (join (fmap join as) :: TwoFingerOddA Int Int)
  , testProperty "join . pure === id" $ \(AnyOddA as) ->
      as == (join (pure as) :: TwoFingerOddA Int Int)
  , testProperty "join . fmap pure === id" $ \(AnyOddA as) ->
      as == (join (fmap pure as) :: TwoFingerOddA Int Int)
  ]

--TODO: test isTraversal on the various traversals? Is there an isTraversal1? isTraversal for the bitraversals??
lensProperties :: TestTree
lensProperties = testGroup "lens laws"
  [ testProperty "firstOddA" $ isLens $ (_AnyOddA . firstOddA :: Lens' (AnyOddA Int Int) Int)
  , testProperty "lastOddA" $ isLens $ (_AnyOddA . lastOddA :: Lens' (AnyOddA Int Int) Int)
  ]

main :: IO ()
main = defaultMain $ testGroup "property tests"
  [ halfconsProperties
  , associativeProperties
  , monoidIdentityProperties
  , monadProperties
  , lensProperties
  ]
