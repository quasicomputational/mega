{-# LANGUAGE TemplateHaskell #-}
--TODO: It's nice being able to edit the properties as code, which we couldn't have if they were haddocks, but it'd also be nice to have some way to cross-link them to the relevant functions in the Q4C12.TwoFinger's generated haddocks; maybe even to generate pretty documentation of the properties?
module Main
  ( main
  )
  where

import Control.Lens (Lens', makePrisms)
import Control.Lens.Properties (isLens)
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Semigroup ((<>))
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck (Gen, Arbitrary, testProperty)
import qualified Test.Tasty.QuickCheck as QC

import Q4C12.TwoFinger.Internal (Digit (One, Two, Three, Four), TwoFingerOddA (SingleOddA, EmptyOddA, DeepOddA), TwoFingerOddE (SingleOddE, DeepOddE), TwoFingerEvenA (SingleEvenA, EmptyEvenA, DeepEvenA), TwoFingerEvenE (SingleEvenE, EmptyEvenE, DeepEvenE), Node (Node2, Node3), unconsEvenA, unconsEvenE, digitToTree, halfsnocOddA, halfsnocOddE, halfunsnocOddA, halfconsEvenE, halfunconsOddA, halfconsOddE, halfsnocEvenA, repeatEvenA, repeatEvenE, repeatOddA, repeatOddE, alignLeftOddAOddA, alignLeftOddAEvenA, alignLeftOddEOddE, alignLeftOddEEvenE, alignLeftEvenAEvenA, alignLeftEvenEEvenE, appendOddEOddA, appendEvenEOddE, appendOddEEvenA, appendOddAOddE, appendEvenAOddA, appendOddAEvenE, halfunsnocEvenA, halfunsnocEvenE, halfsnocEvenE, halfunconsEvenA, halfunsnocOddE, halfunconsEvenE, halfconsOddA, halfunconsOddE, halfconsEvenA, firstOddA, lastOddA)

genDigit :: Gen e -> Gen a -> Gen (Digit e a)
genDigit e a = QC.oneof
  [ One <$> e
  , Two <$> e <*> a <*> e
  , Three <$> e <*> a <*> e <*> a <*> e
  , Four <$> e <*> a <*> e <*> a <*> e <*> a <*> e
  ]

genNode :: Gen e -> Gen a -> Gen (Node e a)
genNode e a = QC.oneof
  [ Node2 <$> e <*> a <*> e
  , Node3 <$> e <*> a <*> e <*> a <*> e
  ]

-- | The 'Int' parameter is expontential size: for a value \(n\), the generated tree will have (slightly more than) \(2^(n/2)\) to \(3^(n/2)\) elements.
genOddA :: Gen e -> Gen a -> Int -> Gen (TwoFingerOddA e a)
genOddA e a 1 = SingleOddA <$> a <*> e <*> a
genOddA _ a n | n <= 0 = EmptyOddA <$> a
genOddA e a n =
  DeepOddA <$> a <*> genDigit e a <*> genOddA (genNode e a) a (n - 2) <*> genDigit e a <*> a

--TODO: better shrinks? This isn't wrong, and it's better than the default, but we could be doing better (e.g., trying just the middle tree in Deep; also possibly just dropping things off the ends...).
shrinkOddA :: TwoFingerOddA e a -> [TwoFingerOddA e a]
shrinkOddA = \case
  EmptyOddA _ -> []
  SingleOddA a1 _ a2 ->
    [ EmptyOddA a1
    , EmptyOddA a2
    ]
  DeepOddA a0 pr m sf a1 -> mconcat
    [ [ halfsnocEvenA (halfconsOddE a0 $ digitToTree pr) (fst $ halfunconsOddA m)
      , halfconsEvenE (snd $ halfunsnocOddA m) (halfsnocOddE (digitToTree sf) a1)
      ]
    , [EmptyOddA a0]
    , [EmptyOddA a1]
    , (\m' -> DeepOddA a0 pr m' sf a1) <$> shrinkOddA m
    ]

shrinkOddE :: TwoFingerOddE e a -> [TwoFingerOddE e a]
shrinkOddE (SingleOddE _) = []
shrinkOddE (DeepOddE pr m sf) = (\m' -> DeepOddE pr m' sf) <$> shrinkOddA m

shrinkEvenA :: TwoFingerEvenA e a -> [TwoFingerEvenA e a]
shrinkEvenA tree = case unconsEvenA tree of
  Nothing -> []
  Just (_, tree') -> [tree']

shrinkEvenE :: TwoFingerEvenE e a -> [TwoFingerEvenE e a]
shrinkEvenE tree = case unconsEvenE tree of
  Nothing -> []
  Just (_, tree') -> [tree']

newtype AnyOddA e a = AnyOddA { getAnyOddA :: TwoFingerOddA e a }
  deriving (Show, Eq)

instance (Arbitrary e, Arbitrary a) => Arbitrary (AnyOddA e a) where
  arbitrary = fmap AnyOddA $ genOddA QC.arbitrary QC.arbitrary =<< QC.choose (0, 10)
  shrink = fmap AnyOddA . shrinkOddA . getAnyOddA

newtype AnyOddE e a = AnyOddE { getAnyOddE :: TwoFingerOddE e a }
  deriving (Show, Eq)

instance (Arbitrary e, Arbitrary a) => Arbitrary (AnyOddE e a) where
  arbitrary = AnyOddE <$> QC.oneof
    [ SingleOddE <$> QC.arbitrary
    , DeepOddE <$> genDigit QC.arbitrary QC.arbitrary <*> (genOddA (genNode QC.arbitrary QC.arbitrary) QC.arbitrary =<< QC.choose (0, 10)) <*> genDigit QC.arbitrary QC.arbitrary
    ]
  shrink = fmap AnyOddE . shrinkOddE . getAnyOddE

newtype AnyEvenA e a = AnyEvenA { getAnyEvenA :: TwoFingerEvenA e a }
  deriving (Show, Eq)

instance (Arbitrary e, Arbitrary a) => Arbitrary (AnyEvenA e a) where
  arbitrary = AnyEvenA <$> QC.oneof
    [ pure EmptyEvenA
    , SingleEvenA <$> QC.arbitrary <*> QC.arbitrary
    , DeepEvenA <$> QC.arbitrary <*> genDigit QC.arbitrary QC.arbitrary <*> (genOddA (genNode QC.arbitrary QC.arbitrary) QC.arbitrary =<< QC.choose (0, 10)) <*> genDigit QC.arbitrary QC.arbitrary
    ]
  shrink = fmap AnyEvenA . shrinkEvenA . getAnyEvenA

newtype AnyEvenE e a = AnyEvenE { getAnyEvenE :: TwoFingerEvenE e a }
  deriving (Show, Eq)

instance (Arbitrary e, Arbitrary a) => Arbitrary (AnyEvenE e a) where
  arbitrary = AnyEvenE <$> QC.oneof
    [ pure EmptyEvenE
    , SingleEvenE <$> QC.arbitrary <*> QC.arbitrary
    , DeepEvenE <$> genDigit QC.arbitrary QC.arbitrary <*> (genOddA (genNode QC.arbitrary QC.arbitrary) QC.arbitrary =<< QC.choose (0, 10)) <*> genDigit QC.arbitrary QC.arbitrary <*> QC.arbitrary
    ]
  shrink = fmap AnyEvenE . shrinkEvenE . getAnyEvenE

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

alignProperties :: TestTree
alignProperties = testGroup "aligning is lossless"
  [ testProperty "OddA OddA" $ intFields $ \(AnyOddA as) -> intFields $ \(AnyOddA bs) ->
      let (aligned, rest) = alignLeftOddAOddA as bs
          as' = appendOddAEvenE (bimap fst fst aligned) (either id (const mempty) rest)
          bs' = appendOddAEvenE (bimap snd snd aligned) (either (const mempty) id rest)
      in as == as' && bs == bs'
  , testProperty "OddA EvenA" $ intFields $ \(AnyOddA as) -> intFields $ \(AnyEvenA bs) ->
      let (as', bs') = case alignLeftOddAEvenA as bs of
            Left (aligned, rest) ->
              (appendEvenAOddA (bimap fst fst aligned) rest, bimap snd snd aligned)
            Right (aligned, rest) ->
              (bimap fst fst aligned, appendOddAOddE (bimap snd snd aligned) rest)
      in as == as' && bs == bs'
  , testProperty "OddE OddE" $ intFields $ \(AnyOddE as) -> intFields $ \(AnyOddE bs) ->
      let (aligned, rest) = alignLeftOddEOddE as bs
          as' = appendOddEEvenA (bimap fst fst aligned) (either id (const mempty) rest)
          bs' = appendOddEEvenA (bimap snd snd aligned) (either (const mempty) id rest)
      in as == as' && bs == bs'
  , testProperty "OddE EvenE" $ intFields $ \(AnyOddE as) -> intFields $ \(AnyEvenE bs) ->
      let (as', bs') = case alignLeftOddEEvenE as bs of
            Left (aligned, rest) ->
              (appendEvenEOddE (bimap fst fst aligned) rest, bimap snd snd aligned)
            Right (aligned, rest) ->
              (bimap fst fst aligned, appendOddEOddA (bimap snd snd aligned) rest)
      in as == as' && bs == bs'
  ]

alignIdentityProperties :: TestTree
alignIdentityProperties = testGroup "align identities"
  [ testProperty "left OddA OddA" $ intFields $ \(AnyOddA as) ->
      as == bimap (uncurry ($)) (uncurry ($)) (fst $ alignLeftOddAOddA (repeatOddA id id) as)
  , testProperty "right OddA OddA" $ intFields $ \(AnyOddA as) ->
      as == bimap (uncurry $ flip ($)) (uncurry $ flip ($)) (fst $ alignLeftOddAOddA as (repeatOddA id id))
  , testProperty "left OddA EvenA" $ intFields $ \(AnyEvenA as) ->
      either ((as ==) . bimap (uncurry ($)) (uncurry ($)) . fst) (const False) (alignLeftOddAEvenA (repeatOddA id id) as)
  , testProperty "right OddA EvenA" $ intFields $ \(AnyOddA as) ->
      either (const False) ((==) as . bimap (uncurry $ flip ($)) (uncurry $ flip ($)) . fst) $ alignLeftOddAEvenA as (repeatEvenA id id)
  , testProperty "left OddE OddE" $ intFields $ \(AnyOddE as) ->
      as == bimap (uncurry ($)) (uncurry ($)) (fst $ alignLeftOddEOddE (repeatOddE id id) as)
  , testProperty "right OddE OddE" $ intFields $ \(AnyOddE as) ->
      as == bimap (uncurry $ flip ($)) (uncurry $ flip ($)) (fst $ alignLeftOddEOddE as (repeatOddE id id))
  , testProperty "left OddE EvenE" $ intFields $ \(AnyEvenE as) ->
      either ((==) as . bimap (uncurry ($)) (uncurry ($)) . fst) (const False) $ alignLeftOddEEvenE (repeatOddE id id) as
  , testProperty "right OddE EvenE" $ intFields $ \(AnyOddE as) ->
      either (const False) ((==) as . bimap (uncurry $ flip ($)) (uncurry $ flip ($)) . fst) $ alignLeftOddEEvenE as (repeatEvenE id id)
  , testProperty "left EvenA EvenA" $ intFields $ \(AnyEvenA as) ->
      as == bimap (uncurry ($)) (uncurry ($)) (fst $ alignLeftEvenAEvenA (repeatEvenA id id) as)
  , testProperty "right EvenA EvenA" $ intFields $ \(AnyEvenA as) ->
      as == bimap (uncurry $ flip ($)) (uncurry $ flip ($)) (fst $ alignLeftEvenAEvenA as (repeatEvenA id id))
  , testProperty "left EvenE EvenE" $ intFields $ \(AnyEvenE as) ->
      as == bimap (uncurry ($)) (uncurry ($)) (fst $ alignLeftEvenEEvenE (repeatEvenE id id) as)
  , testProperty "right EvenE EvenE" $ intFields $ \(AnyEvenE as) ->
      as == bimap (uncurry $ flip ($)) (uncurry $ flip ($)) (fst $ alignLeftEvenEEvenE as (repeatEvenE id id))
  ]

monadProperties :: TestTree
monadProperties = testGroup "OddA monad laws"
  [ testProperty "join . join === join . fmap join" $
      --Since we generate 3 layers deep, the things can get big with the default settings.
      let gen :: Gen a -> Gen (TwoFingerOddA Int a)
          gen a = genOddA QC.arbitrary a =<< QC.choose (0, 3)
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

--TODO: 'Associativity' properties for align*.
--TODO: Infinite trees? Some of the properties ought to be able to handle it!
main :: IO ()
main = defaultMain $ testGroup "property tests"
  [ halfconsProperties
  , associativeProperties
  , monoidIdentityProperties
  , alignProperties
  , alignIdentityProperties
  , monadProperties
  , lensProperties
  ]

