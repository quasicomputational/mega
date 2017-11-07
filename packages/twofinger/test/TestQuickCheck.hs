module Main
  ( main
  )
  where

import Control.Monad (unless)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor.Classes (Eq1, liftEq)
import Data.Semigroup ((<>))
import Lens.Micro (over)
import Lens.Micro.Extras (view)
import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.QuickCheck
  (testProperty, Arbitrary (arbitrary, shrink), Gen, oneof, choose)

import Q4C12.TwoFinger
  ( TwoFingerOddA, singletonOddA, firstOddA, lastOddA, halfsnocEvenA
  , halfconsEvenE, halfconsOddE, halfsnocOddE, halfconsOddA, halfunconsEvenE
  , halfconsEvenA, halfunconsOddE, unconsEvenA
  , halfunsnocOddA, halfsnocOddA, halfunsnocEvenA
  )
import Q4C12.TwoFinger.Internal
  ( TwoFingerOddA (EmptyOddA, SingleOddA, DeepOddA), Node (Node2, Node3)
  , Digit (One, Two, Three, Four), digitToTree
  , TwoFingerEvenA (EmptyEvenA, SingleEvenA, DeepEvenA)
  )

genOddA :: Gen e -> Gen a -> Int -> Gen (TwoFingerOddA e a)
genOddA _ a 0 = EmptyOddA <$> a
genOddA e a 1 = SingleOddA <$> a <*> e <*> a
genOddA e a n = do
  unless (n > 0) $ fail "genOddA called with negative size parameter."
  a0 <- a
  a1 <- a
  pr <- genDigit e a
  sf <- genDigit e a
  m <- genOddA (genNode e a) a (n - 2)
  pure $ DeepOddA a0 pr m sf a1

genDigit :: Gen e -> Gen a -> Gen (Digit e a)
genDigit e a = oneof
  [ One <$> e
  , Two <$> e <*> a <*> e
  , Three <$> e <*> a <*> e <*> a <*> e
  , Four <$> e <*> a <*> e <*> a <*> e <*> a <*> e
  ]

genNode :: Gen e -> Gen a -> Gen (Node e a)
genNode e a = oneof
  [ Node2 <$> e <*> a <*> e
  , Node3 <$> e <*> a <*> e <*> a <*> e
  ]

newtype OA = OA { getOA :: TwoFingerOddA Int (DList Int) }
  deriving (Show)

instance Arbitrary OA where
  arbitrary = fmap OA $ genOddA arbitrary (DList.singleton <$> arbitrary) =<< choose (0, 10)
  shrink = fmap OA . shrinkOddA . getOA

--TODO: better shrinks? This isn't wrong, and it's better than the default, but we could be doing better (e.g., trying just the middle tree in Deep).
shrinkOddA :: TwoFingerOddA e a -> [TwoFingerOddA e a]
shrinkOddA = \case
  EmptyOddA _ -> []
  SingleOddA a1 _ a2 ->
    [ EmptyOddA a1
    , EmptyOddA a2
    ]
  DeepOddA a0 pr m sf a1 -> mconcat
    [ [ halfsnocEvenA (halfconsOddE a0 $ digitToTree pr) (view firstOddA m)
      , halfconsEvenE (view lastOddA m) (halfsnocOddE (digitToTree sf) a1)
      ]
    , [EmptyOddA a0]
    , [EmptyOddA a1]
    , (\m' -> DeepOddA a0 pr m' sf a1) <$> shrinkOddA m
    ]

newtype EA = EA { getEA :: TwoFingerEvenA Int (DList Int) }
  deriving (Show)

instance Arbitrary EA where
  arbitrary = fmap EA $ oneof
    [ pure EmptyEvenA
    , SingleEvenA <$> a <*> e
    , DeepEvenA <$> a <*> genDigit e a <*> (genOddA (genNode e a) a =<< choose (0, 10)) <*> genDigit e a
    ]
    where
      e :: Gen Int
      e = arbitrary
      a :: Gen (DList Int)
      a = DList.singleton <$> arbitrary
  shrink = fmap EA . shrinkEA . getEA

shrinkEA :: TwoFingerEvenA e a -> [TwoFingerEvenA e a]
shrinkEA tree = case unconsEvenA tree of
  Nothing -> []
  Just (_, _, tree') -> [tree']

eqT :: (Eq1 f, Eq a) => f (DList a) -> f (DList a) -> Bool
eqT = liftEq $ on (==) toList

--TODO: test more properties
main :: IO ()
main =  defaultMain $ testGroup "twofinger property tests"
  [ testGroup "TwoFingerOddA"
    [ testProperty "semigroup association" $ \(OA a) (OA b) (OA c) ->
        eqT ((a <> b) <> c) (a <> (b <> c))
    , testProperty "append singleton" $ \(OA a) ->
        eqT (a <> singletonOddA (DList.singleton 0)) (over lastOddA (<> DList.singleton 0) a)
    , testProperty "monoid left" $ \(OA a) ->
        eqT a (mempty <> a)
    , testProperty "monoid right" $ \(OA a) ->
        eqT a (a <> mempty)
    ]
  , testGroup "TwoFingerEvenA"
    [ testProperty "semigroup association" $ \(EA a) (EA b) (EA c) ->
        eqT ((a <> b) <> c) (a <> (b <> c))
    , testProperty "monoid left" $ \(EA a) ->
        eqT a (mempty <> a)
    , testProperty "monoid right" $ \(EA a) ->
        eqT a (a <> mempty)
    ]
  , testGroup "halfcons/snoc inverses"
    [ testProperty "consOddA" $ \e (OA xs) ->
        let xs' = toList <$> xs
        in Just (e, xs') == halfunconsEvenE (halfconsOddA e xs')
    , testProperty "snocOddA" $ \e (OA xs) ->
        let xs' = toList <$> xs
        in Just (xs', e) == halfunsnocEvenA (halfsnocOddA xs' e)
    , testProperty "consEvenA" $ \e (EA xs) ->
        let xs' = toList <$> xs
        in (e, xs') == halfunconsOddE (halfconsEvenA e xs')
    , testProperty "snocEvenA" $ \a (EA xs) ->
        let xs' = toList <$> xs
        in (xs', a) == halfunsnocOddA (halfsnocEvenA xs' a)
    ]
  ]
