{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
-- |
-- Stability: unstable
--
-- This is an __internal module__ and __not subject to the PVP__. It
-- may receive arbitrary changes at any time and between any two
-- releases. Import from "Q4C12.TwoFinger" instead, unless you really
-- need the gory details, and, in that case, you must depend on the
-- __exact__ version of this package. (If you do need them, please
-- file a bug so that, hopefully, your use-case can be accomplished
-- through the public interface.)

module Q4C12.TwoFinger.Internal where

import Control.DeepSeq (NFData)
import Control.Monad (ap)
import Data.Bifunctor (Bifunctor (bimap), first, second)
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bitraversable
  (Bitraversable (bitraverse), bifoldMapDefault, bimapDefault)
import Data.Data (Data)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply
  ( Apply, (<.>), MaybeApply (MaybeApply)
  , WrappedApplicative (WrapApplicative), unwrapApplicative
  )
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Classes
  ( Eq2 (liftEq2), Eq1 (liftEq), eq2, Show2 (liftShowsPrec2)
  , Show1 (liftShowsPrec), showsPrec2
  )
import Data.Functor.Plus (Plus (zero))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Semigroup ((<>)))
import Data.Semigroup.Bifoldable (Bifoldable1 (bifoldMap1))
import Data.Semigroup.Bitraversable
  (Bitraversable1 (bitraverse1), bifoldMap1Default)
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Semigroup.Traversable
  (Traversable1 (traverse1), foldMap1Default)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (foldMapDefault, fmapDefault)
import GHC.Generics (Generic)

-- $setup
-- >>> import Data.List (unfoldr)
-- >>> import Data.Tuple (swap)
-- >>> import Control.Lens (over, view)
-- >>> let hush = either (const Nothing) Just

--TODO: Fill in the gaps in the API.

--TODO: Flipped TwoFingerEvenA has a sensible Alt/Plus instance. So,
--maybe offer a wholly flipped set of flavours?

--TODO: Alternative zippy Applicatives instances.

--TODO: Consider exporting bits and pieces from, e.g., Q4C12.TwoFinger.EvenA, without the flavour-identifying suffix, to allow qualified import.

--TODO: the issue with the mathy haddocks is that double-clicking on a paragraph with one of them in them won't select the whole paragraph.

--TODO: the tuples are annoying. Consider moving to HLists.

--TODO: revise the naming scheme of functions? In particular, singletonOddA -> emptyOddA??

--TODO: efficient unzips? The fmap-based approach can be a space leak.

--TODO: send this upstream to semigroupoids? Opened issue: https://github.com/ekmett/semigroupoids/issues/66
(<.*>) :: (Apply f) => f (a -> b) -> MaybeApply f a -> f b
ff <.*> MaybeApply (Left fa) = ff <.> fa
ff <.*> MaybeApply (Right a) = ($ a) <$> ff
infixl 4 <.*>

(<*.>) :: (Apply f) => MaybeApply f (a -> b) -> f a -> f b
MaybeApply (Left ff) <*.> fa = ff <.> fa
MaybeApply (Right f) <*.> fa = f <$> fa
infixl 4 <*.>

traverseDefault
  :: (Applicative f, Traversable1 t) => (a -> f a') -> t a -> f (t a')
traverseDefault f = unwrapApplicative . traverse1 (WrapApplicative . f)

bitraverseDefault
  :: (Applicative f, Bitraversable1 t)
  => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
bitraverseDefault f g =
  unwrapApplicative . bitraverse1 (WrapApplicative . f) (WrapApplicative . g)

--Only needed for containers prior to 0.5.9.1, which are boot libraries for GHCs < 8.2. TODO: rm once they're out of the support window.
liftEqSeq :: (a -> b -> Bool) -> Seq a -> Seq b -> Bool
liftEqSeq f as bs = case (Seq.viewl as, Seq.viewl bs) of
  (Seq.EmptyL, Seq.EmptyL) -> True
  (Seq.EmptyL, _ Seq.:< _) -> False
  (_ Seq.:< _, Seq.EmptyL) -> False
  (a Seq.:< as', b Seq.:< bs') -> f a b && liftEqSeq f as' bs'

-- * Types, EqN?\/ShowN?\/(Bi)Functor\/Foldable1?\/Traversable1?
-- instances, and odd traversals.

-- | Isomorphic to @a, (e, a)*@
data TwoFingerOddA e a = TwoFingerOddA (Seq (a, e)) a
  deriving (Generic, Data)

instance Show2 TwoFingerOddA where
  liftShowsPrec2 f _ g _ d = go (d > 10)
    where
      go paren tree = showParen paren $ case unconsOddA tree of
        Left a -> showString "singletonOddA " . g 11 a
        Right ((a, e), tree')
          -> showString "consOddA "
           . g 11 a . showString " "
           . f 11 e . showString " "
           . go True tree'

instance (Show e) => Show1 (TwoFingerOddA e) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show e, Show a) => Show (TwoFingerOddA e a) where
  showsPrec = showsPrec2

instance Eq2 TwoFingerOddA where
  liftEq2 f g (TwoFingerOddA as a) (TwoFingerOddA bs b) =
    liftEqSeq (liftEq2 g f) as bs && g a b

instance (Eq e) => Eq1 (TwoFingerOddA e) where
  liftEq = liftEq2 (==)

instance (Eq e, Eq a) => Eq (TwoFingerOddA e a) where
  (==) = eq2

instance (NFData e, NFData a) => NFData (TwoFingerOddA e a)

--TODO: If we had 'type>', we could document the lensiness directly.
--See https://github.com/sol/doctest/issues/153
-- | Access the first @a@ of a @'TwoFingerOddA' e a@. \(O(1)\) amortised, \(O(\log n)\) worst case. This
-- type is @Lens' ('TwoFingerOddA' e a) a@ in disguise.
--
-- >>> view firstOddA (consOddA 3 True $ singletonOddA 15)
-- 3
firstOddA
  :: (Functor f) => (a -> f a) -> TwoFingerOddA e a -> f (TwoFingerOddA e a)
firstOddA f (halfunconsOddA -> (a, tree)) = flip halfconsEvenE tree <$> f a

-- | Access the last @a@ of a @'TwoFingerOddA' e a@. \(O(1)\). This type
-- is @Lens' ('TwoFingerOddA' e a) a@ in disguise.
--
-- >>> over lastOddA (+ 5) (consOddA 3 True $ singletonOddA 15)
-- consOddA 3 True (singletonOddA 20)
lastOddA
  :: (Functor f) => (a -> f a) -> TwoFingerOddA e a -> f (TwoFingerOddA e a)
lastOddA f (halfunsnocOddA -> (tree, a)) = halfsnocEvenA tree <$> f a

instance Functor (TwoFingerOddA e) where
  fmap = fmapDefault

instance Foldable (TwoFingerOddA e) where
  foldMap = foldMapDefault

instance Foldable1 (TwoFingerOddA e) where
  foldMap1 = foldMap1Default

instance Traversable (TwoFingerOddA e) where
  traverse = bitraverse pure

instance Traversable1 (TwoFingerOddA e) where
  traverse1 f (TwoFingerOddA as a) = TwoFingerOddA
     <$> traverse (bitraverse (MaybeApply . Left . f) pure) as
    <*.> f a

instance Bifunctor TwoFingerOddA where
  bimap = bimapDefault

instance Bifoldable TwoFingerOddA where
  bifoldMap = bifoldMapDefault

instance Bifoldable1 TwoFingerOddA where
  bifoldMap1 = bifoldMap1Default

instance Bitraversable TwoFingerOddA where
  bitraverse = bitraverseDefault

instance Bitraversable1 TwoFingerOddA where
  bitraverse1 f g (TwoFingerOddA as a) = TwoFingerOddA
     <$> traverse (MaybeApply . Left . bitraverse1 g f) as
    <*.> g a

-- | Isomorphic to @e, (a, e)*@
data TwoFingerOddE e a = TwoFingerOddE e (Seq (a, e))
  deriving (Generic, Data)

instance Show2 TwoFingerOddE where
  liftShowsPrec2 f _ g _ d = go (d > 10)
    where
      go paren tree = showParen paren $ case unconsOddE tree of
        Left e -> showString "singletonOddE " . f 11 e
        Right ((e, a), tree')
          -> showString "consOddE "
           . f 11 e . showString " "
           . g 11 a . showString " "
           . go True tree'

instance (Show e) => Show1 (TwoFingerOddE e) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show e, Show a) => Show (TwoFingerOddE e a) where
  showsPrec = showsPrec2

instance Eq2 TwoFingerOddE where
  liftEq2 f g (TwoFingerOddE a as) (TwoFingerOddE b bs) =
    liftEqSeq (liftEq2 g f) as bs && f a b

instance (Eq e) => Eq1 (TwoFingerOddE e) where
  liftEq = liftEq2 (==)

instance (Eq e, Eq a) => Eq (TwoFingerOddE e a) where
  (==) = eq2

instance Functor (TwoFingerOddE e) where
  fmap = bimap id

instance Foldable (TwoFingerOddE e) where
  foldMap = bifoldMap mempty

instance Traversable (TwoFingerOddE e) where
  traverse = bitraverse pure

instance Bifunctor TwoFingerOddE where
  bimap = bimapDefault

instance Bifoldable TwoFingerOddE where
  bifoldMap = bifoldMapDefault

instance Bifoldable1 TwoFingerOddE where
  bifoldMap1 = bifoldMap1Default

instance Bitraversable TwoFingerOddE where
  bitraverse = bitraverseDefault

instance Bitraversable1 TwoFingerOddE where
  bitraverse1 f g (TwoFingerOddE a as) = TwoFingerOddE
     <$> f a
    <.*> traverse (MaybeApply . Left . bitraverse1 g f) as

instance (NFData e, NFData a) => NFData (TwoFingerOddE e a)

--TODO: cleaner to offer TwoFingerEvenE1, without EmptyL?
-- | Isomorphic to @(e, a)*@
data TwoFingerEvenE e a
  = EmptyEvenE
  | TwoFingerEvenE e (Seq (a, e)) a
  deriving (Generic, Data)

instance Show2 TwoFingerEvenE where
  liftShowsPrec2 f _ g _ d = go (d > 10)
    where
      go paren tree = case unconsEvenE tree of
        Nothing -> showString "mempty"
        Just ((e, a), tree') -> showParen paren
          $ showString "consEvenE "
          . f 11 e
          . showString " "
          . g 11 a
          . showString " "
          . go True tree'

instance (Show e) => Show1 (TwoFingerEvenE e) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show e, Show a) => Show (TwoFingerEvenE e a) where
  showsPrec = showsPrec2

instance Eq2 TwoFingerEvenE where
  liftEq2 _ _ EmptyEvenE EmptyEvenE = True
  liftEq2 _ _ EmptyEvenE (TwoFingerEvenE {}) = False
  liftEq2 _ _ (TwoFingerEvenE {}) EmptyEvenE = False
  liftEq2 f g (TwoFingerEvenE a as e) (TwoFingerEvenE a' as' e') =
    g e e' && f a a' && liftEqSeq (liftEq2 g f) as as'

instance (Eq e) => Eq1 (TwoFingerEvenE e) where
  liftEq = liftEq2 (==)

instance (Eq e, Eq a) => Eq (TwoFingerEvenE e a) where
  (==) = eq2

instance (NFData e, NFData a) => NFData (TwoFingerEvenE e a)

instance Functor (TwoFingerEvenE e) where
  fmap = fmapDefault

instance Foldable (TwoFingerEvenE e) where
  foldMap = foldMapDefault

instance Traversable (TwoFingerEvenE e) where
  traverse = bitraverse pure

instance Bifunctor TwoFingerEvenE where
  bimap = bimapDefault

instance Bifoldable TwoFingerEvenE where
  bifoldMap = bifoldMapDefault

instance Bitraversable TwoFingerEvenE where
  bitraverse _ _ EmptyEvenE = pure EmptyEvenE
  bitraverse f g (TwoFingerEvenE e as a) = TwoFingerEvenE
    <$> f e
    <*> traverse (bitraverse g f) as
    <*> g a

-- | Isomorphic to @(a, e)*@
data TwoFingerEvenA e a = TwoFingerEvenA (Seq (a, e))
  deriving (Generic, Data)

instance Show2 TwoFingerEvenA where
  liftShowsPrec2 f _ g _ d = go (d > 10)
    where
      go paren tree = case unconsEvenA tree of
        Nothing -> showString "mempty"
        Just ((a, e), tree') -> showParen paren
          $ showString "consEvenA "
          . g 11 a . showString " "
          . f 11 e . showString " "
          . go True tree'

instance (Show e) => Show1 (TwoFingerEvenA e) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show e, Show a) => Show (TwoFingerEvenA e a) where
  showsPrec = showsPrec2

instance Eq2 TwoFingerEvenA where
  liftEq2 f g (TwoFingerEvenA as) (TwoFingerEvenA bs) =
    liftEqSeq (liftEq2 g f) as bs

instance (Eq e) => Eq1 (TwoFingerEvenA e) where
  liftEq = liftEq2 (==)

instance (Eq e, Eq a) => Eq (TwoFingerEvenA e a) where
  (==) = eq2

instance (NFData e, NFData a) => NFData (TwoFingerEvenA e a)

instance Functor (TwoFingerEvenA e) where
  fmap = fmapDefault

instance Foldable (TwoFingerEvenA e) where
  foldMap = foldMapDefault

instance Traversable (TwoFingerEvenA e) where
  traverse = bitraverse pure

instance Bifunctor TwoFingerEvenA where
  bimap = bimapDefault

instance Bifoldable TwoFingerEvenA where
  bifoldMap = bifoldMapDefault

instance Bitraversable TwoFingerEvenA where
  bitraverse f g (TwoFingerEvenA as) = TwoFingerEvenA <$> traverse (bitraverse g f) as

-- * (Un)conses/snocs for TwoFingerOddA.
consOddA :: a -> e -> TwoFingerOddA e a -> TwoFingerOddA e a
consOddA a e = halfconsEvenE a . halfconsOddA e

snocOddA :: TwoFingerOddA e a -> e -> a -> TwoFingerOddA e a
snocOddA tree e = halfsnocEvenA (halfsnocOddA tree e)

unconsOddA :: TwoFingerOddA e a -> Either a ((a, e), TwoFingerOddA e a)
unconsOddA tree = case second halfunconsEvenE $ halfunconsOddA tree of
  (a, Nothing) -> Left a
  (a, Just (e, tree')) -> Right ((a, e), tree')

unsnocOddA :: TwoFingerOddA e a -> Either a (TwoFingerOddA e a, (e, a))
unsnocOddA tree = case first halfunsnocEvenA $ halfunsnocOddA tree of
  (Nothing, a) -> Left a
  (Just (tree', e), a) -> Right (tree', (e, a))

-- | \(O(1)\) worst case. Inverse: 'halfunconsEvenE'
halfconsOddA :: e -> TwoFingerOddA e a -> TwoFingerEvenE e a
halfconsOddA e (TwoFingerOddA as a) = TwoFingerEvenE e as a

-- | \(O(\log n)\) worst case. Inverse: 'halfunsnocEvenA'
halfsnocOddA :: TwoFingerOddA e a -> e -> TwoFingerEvenA e a
halfsnocOddA (TwoFingerOddA as a) e = TwoFingerEvenA $ as Seq.|> (a, e)

-- | \(O(\log n)\) worst case. Inverse: 'halfconsEvenE'
halfunconsOddA :: TwoFingerOddA e a -> (a, TwoFingerEvenE e a)
halfunconsOddA (TwoFingerOddA as a) = case Seq.viewl as of
  Seq.EmptyL -> (a, mempty)
  (a', e') Seq.:< as' -> (a', TwoFingerEvenE e' as' a)

-- | \(O(1)\) worst case. Inverse: 'halfsnocEvenA'
halfunsnocOddA :: TwoFingerOddA e a -> (TwoFingerEvenA e a, a)
halfunsnocOddA (TwoFingerOddA as a) = (TwoFingerEvenA as, a)

-- * (Un)conses/snocs for TwoFingerOddE.
consOddE :: e -> a -> TwoFingerOddE e a -> TwoFingerOddE e a
consOddE e a = halfconsEvenA e . halfconsOddE a

snocOddE :: TwoFingerOddE e a -> a -> e -> TwoFingerOddE e a
snocOddE tree e = halfsnocEvenE (halfsnocOddE tree e)

unconsOddE :: TwoFingerOddE e a -> Either e ((e, a), TwoFingerOddE e a)
unconsOddE tree = case second halfunconsEvenA $ halfunconsOddE tree of
  (e, Nothing) -> Left e
  (e, Just (a, tree')) -> Right ((e, a), tree')

unsnocOddE :: TwoFingerOddE e a -> Either e (TwoFingerOddE e a, (a, e))
unsnocOddE tree = case first halfunsnocEvenE $ halfunsnocOddE tree of
  (Nothing, e) -> Left e
  (Just (tree', a), e) -> Right (tree', (a, e))

-- | \(O(\log n)\) worst case. Inverse: 'halfunconsEvenA'
halfconsOddE :: a -> TwoFingerOddE e a -> TwoFingerEvenA e a
halfconsOddE a (TwoFingerOddE e as) = TwoFingerEvenA $ (a, e) Seq.<| as

-- | \(O(1)\) worst case. Inverse: 'halfunsnocEvenE'
halfsnocOddE :: TwoFingerOddE e a -> a -> TwoFingerEvenE e a
halfsnocOddE (TwoFingerOddE e as) a = TwoFingerEvenE e as a

-- | \(O(1)\) worst case. Inverse: 'halfconsEvenA'
halfunconsOddE :: TwoFingerOddE e a -> (e, TwoFingerEvenA e a)
halfunconsOddE (TwoFingerOddE e as) = (e, TwoFingerEvenA as)

-- | \(O(\log n)\) worst case. Inverse: 'halfsnocEvenE'
halfunsnocOddE :: TwoFingerOddE e a -> (TwoFingerEvenE e a, e)
halfunsnocOddE (TwoFingerOddE e as) = case Seq.viewr as of
  Seq.EmptyR -> (mempty, e)
  as' Seq.:> (a', e') -> (TwoFingerEvenE e as' a', e')

-- * (Un)conses/snocs for TwoFingerEvenE.
consEvenE :: e -> a -> TwoFingerEvenE e a -> TwoFingerEvenE e a
consEvenE e a = halfconsOddA e . halfconsEvenE a

snocEvenE :: TwoFingerEvenE e a -> e -> a -> TwoFingerEvenE e a
snocEvenE tree e = halfsnocOddE (halfsnocEvenE tree e)

unconsEvenE :: TwoFingerEvenE e a -> Maybe ((e, a), TwoFingerEvenE e a)
unconsEvenE tree = case second halfunconsOddA <$> halfunconsEvenE tree of
  Nothing -> Nothing
  Just (e, (a, tree')) -> Just ((e, a), tree')

unsnocEvenE :: TwoFingerEvenE e a -> Maybe (TwoFingerEvenE e a, (e, a))
unsnocEvenE tree = case first halfunsnocOddE <$> halfunsnocEvenE tree of
  Nothing -> Nothing
  Just ((tree', a), e) -> Just (tree', (a, e))

-- | \(O(\log n)\) worst case. Inverse: 'halfunconsOddA'
halfconsEvenE :: a -> TwoFingerEvenE e a -> TwoFingerOddA e a
halfconsEvenE a EmptyEvenE = TwoFingerOddA mempty a
halfconsEvenE a (TwoFingerEvenE e as a') = TwoFingerOddA ((a, e) Seq.<| as) a'

-- | \(O(\log n)\) worst case. Inverse: 'halfunsnocOddE'.
halfsnocEvenE :: TwoFingerEvenE e a -> e -> TwoFingerOddE e a
halfsnocEvenE EmptyEvenE e = TwoFingerOddE e mempty
halfsnocEvenE (TwoFingerEvenE e as a') e' = TwoFingerOddE e $ as Seq.|> (a', e')

-- | \(O(1)\) worst case. Inverse: 'halfconsOddA'.
halfunconsEvenE :: TwoFingerEvenE e a -> Maybe (e, TwoFingerOddA e a)
halfunconsEvenE EmptyEvenE = Nothing
halfunconsEvenE (TwoFingerEvenE e as a) = Just (e, TwoFingerOddA as a)

-- | \(O(1)\) worst case. Inverse: 'halfsnocOddE'.
halfunsnocEvenE :: TwoFingerEvenE e a -> Maybe (TwoFingerOddE e a, a)
halfunsnocEvenE EmptyEvenE = Nothing
halfunsnocEvenE (TwoFingerEvenE e as a) = Just (TwoFingerOddE e as, a)

-- * (Un)conses/snocs for TwoFingerEvenA.
consEvenA :: a -> e -> TwoFingerEvenA e a -> TwoFingerEvenA e a
consEvenA a e = halfconsOddE a . halfconsEvenA e

snocEvenA :: TwoFingerEvenA e a -> a -> e -> TwoFingerEvenA e a
snocEvenA tree a = halfsnocOddA (halfsnocEvenA tree a)

unconsEvenA :: TwoFingerEvenA e a -> Maybe ((a, e), TwoFingerEvenA e a)
unconsEvenA tree = case second halfunconsOddE <$> halfunconsEvenA tree of
  Nothing -> Nothing
  Just (a, (e, tree')) -> Just ((a, e), tree')

unsnocEvenA :: TwoFingerEvenA e a -> Maybe (TwoFingerEvenA e a, (a, e))
unsnocEvenA tree = case first halfunsnocOddA <$> halfunsnocEvenA tree of
  Nothing -> Nothing
  Just ((tree', e), a) -> Just (tree', (e, a))

-- | \(O(1)\) worst case. Inverse: 'halfunconsOddE'.
halfconsEvenA :: e -> TwoFingerEvenA e a -> TwoFingerOddE e a
halfconsEvenA e (TwoFingerEvenA as) = TwoFingerOddE e as

-- | \(O(1)\) worst case. Inverse: 'halfunsnocOddA'.
halfsnocEvenA :: TwoFingerEvenA e a -> a -> TwoFingerOddA e a
halfsnocEvenA (TwoFingerEvenA as) a = TwoFingerOddA as a

-- | \(O(\log n)\) worst case. Inverse: 'halfconsOddE'.
halfunconsEvenA :: TwoFingerEvenA e a -> Maybe (a, TwoFingerOddE e a)
halfunconsEvenA (TwoFingerEvenA as) = case Seq.viewl as of
  Seq.EmptyL -> Nothing
  (a, e) Seq.:< as' -> Just (a, TwoFingerOddE e as')

-- | \(O(\log n)\) worst case. Inverse: 'halfsnocOddA'.
halfunsnocEvenA :: TwoFingerEvenA e a -> Maybe (TwoFingerOddA e a, e)
halfunsnocEvenA (TwoFingerEvenA as) = case Seq.viewr as of
  Seq.EmptyR -> Nothing
  as' Seq.:> (a, e) -> Just (TwoFingerOddA as' a, e)

-- * Monad and Applicative instances, and related operations

joinOddA :: TwoFingerOddA (TwoFingerOddE e a) (TwoFingerOddA e a) -> TwoFingerOddA e a
joinOddA (halfunconsOddA -> (a, tree)) = appendOddAEvenE a (joinEvenE tree)

joinOddE :: TwoFingerOddE (TwoFingerOddE e a) (TwoFingerOddA e a) -> TwoFingerOddE e a
joinOddE (halfunconsOddE -> (e, tree)) = appendOddEEvenA e (joinEvenA tree)

joinEvenA :: TwoFingerEvenA (TwoFingerOddE e a) (TwoFingerOddA e a) -> TwoFingerEvenA e a
joinEvenA tree = case halfunconsEvenA tree of
  Nothing -> mempty
  Just (a, tree') -> appendOddAOddE a (joinOddE tree')

joinEvenE :: TwoFingerEvenE (TwoFingerOddE e a) (TwoFingerOddA e a) -> TwoFingerEvenE e a
joinEvenE tree = case halfunconsEvenE tree of
  Nothing -> mempty
  Just (e, tree') -> appendOddEOddA e (joinOddA tree')

instance Monad (TwoFingerOddA e) where
  tree >>= f = joinOddA $ bimap singletonOddE f tree

instance Bind (TwoFingerOddA e) where
  (>>-) = (>>=)

-- | A \'producty\' instance:
--
-- >>> (,) <$> (consOddA 1 "one" $ consOddA 2 "two" $ singletonOddA 3) <*> (consOddA 'a' "foo" $ singletonOddA 'b')
-- consOddA (1,'a') "foo" (consOddA (1,'b') "one" (consOddA (2,'a') "foo" (consOddA (2,'b') "two" (consOddA (3,'a') "foo" (singletonOddA (3,'b'))))))
instance Applicative (TwoFingerOddA e) where
  pure = singletonOddA
  (<*>) = ap

instance Apply (TwoFingerOddA e) where
  (<.>) = (<*>)

--TODO: Polarity considerations demonstrate that Monad/Bind can't work for EvenA/EvenE, and we can't have Applicative because we can't invent an e out of thin air (well, we could with Monoid e). Can we have Apply, though? OddE could have Bind with a Semigroup e constraint.

-- * Construction and deconstruction of TwoFingerOddA.
singletonOddA :: a -> TwoFingerOddA e a
singletonOddA = TwoFingerOddA mempty

-- | Surrounds the argument with 'mempty'.
--
-- >>> unitOddA 3 :: TwoFingerOddA Int String
-- consOddA "" 3 (singletonOddA "")
unitOddA :: (Monoid a, Semigroup a) => e -> TwoFingerOddA e a
unitOddA a = consOddA mempty a mempty

--TODO: this isn't needed any more in q4c12-xml; remove?
-- |
-- >>> onlyOddA (singletonOddA "Hello!")
-- Just "Hello!"
-- >>> onlyOddA (consOddA True 3 $ singletonOddA False)
-- Nothing
onlyOddA :: TwoFingerOddA e a -> Maybe a
onlyOddA (TwoFingerOddA as a) = case Seq.viewl as of
  Seq.EmptyL -> Just a
  _ -> Nothing

-- |
-- >>> interleavingOddA "sep" (3 :| [4, 5])
-- consOddA 3 "sep" (consOddA 4 "sep" (singletonOddA 5))
interleavingOddA :: e -> NonEmpty a -> TwoFingerOddA e a
interleavingOddA sep (a :| as) =
  foldl' (flip snocOddA sep) (singletonOddA a) as

-- * Construction of TwoFingerOddE
singletonOddE :: e -> TwoFingerOddE e a
singletonOddE e = TwoFingerOddE e mempty

-- * Concatenation of TwoFingerOddA.

instance (Semigroup a) => Semigroup (TwoFingerOddA e a) where
  (<>) = appendOddA

instance (Monoid a, Semigroup a) => Monoid (TwoFingerOddA e a) where
  mempty = singletonOddA mempty
  mappend = (<>)

appendOddA
  :: (Semigroup a)
  => TwoFingerOddA e a
  -> TwoFingerOddA e a
  -> TwoFingerOddA e a
appendOddA (TwoFingerOddA as a) (TwoFingerOddA bs z) =
  case Seq.viewl bs of
    Seq.EmptyL -> TwoFingerOddA as (a <> z)
    (b, e) Seq.:< bs' -> TwoFingerOddA (as <> ((a <> b, e) Seq.<| bs')) z

-- * Concatenation of TwoFingerEvenE.

instance Semigroup (TwoFingerEvenE e a) where
  (<>) = appendEvenE

instance Alt (TwoFingerEvenE e) where
  (<!>) = appendEvenE

instance Monoid (TwoFingerEvenE e a) where
  mempty = EmptyEvenE
  mappend = (<>)

instance Plus (TwoFingerEvenE e) where
  zero = EmptyEvenE

appendEvenE :: TwoFingerEvenE e a -> TwoFingerEvenE e a -> TwoFingerEvenE e a
appendEvenE EmptyEvenE m = m
appendEvenE m EmptyEvenE = m
appendEvenE (TwoFingerEvenE e as a) (TwoFingerEvenE e' as' a') =
  TwoFingerEvenE e (as <> ((a, e') Seq.<| as')) a'

-- * Concatenation of TwoFingerEvenA.

instance Semigroup (TwoFingerEvenA e a) where
  (<>) = appendEvenA

instance Alt (TwoFingerEvenA e) where
  (<!>) = appendEvenA

instance Monoid (TwoFingerEvenA e a) where
  mempty = TwoFingerEvenA mempty
  mappend = (<>)

instance Plus (TwoFingerEvenA e) where
  zero = TwoFingerEvenA mempty

appendEvenA :: TwoFingerEvenA e a -> TwoFingerEvenA e a -> TwoFingerEvenA e a
appendEvenA (TwoFingerEvenA as) (TwoFingerEvenA bs) = TwoFingerEvenA (as <> bs)

-- * Monoid actions

appendOddAEvenE :: TwoFingerOddA e a -> TwoFingerEvenE e a -> TwoFingerOddA e a
appendOddAEvenE m EmptyEvenE = m
appendOddAEvenE (TwoFingerOddA as a) (TwoFingerEvenE e bs z) =
  TwoFingerOddA (as <> ((a, e) Seq.<| bs)) z

appendEvenAOddA :: TwoFingerEvenA e a -> TwoFingerOddA e a -> TwoFingerOddA e a
appendEvenAOddA (TwoFingerEvenA as) (TwoFingerOddA bs z) =
  TwoFingerOddA (as <> bs) z

appendOddAOddE :: TwoFingerOddA e a -> TwoFingerOddE e a -> TwoFingerEvenA e a
appendOddAOddE (TwoFingerOddA as a) (TwoFingerOddE e bs) =
  TwoFingerEvenA (as <> ((a, e) Seq.<| bs))

appendOddEOddA :: TwoFingerOddE e a -> TwoFingerOddA e a -> TwoFingerEvenE e a
appendOddEOddA (TwoFingerOddE e as) (TwoFingerOddA bs a) =
  TwoFingerEvenE e (as <> bs) a

appendOddEEvenA :: TwoFingerOddE e a -> TwoFingerEvenA e a -> TwoFingerOddE e a
appendOddEEvenA (TwoFingerOddE e as) (TwoFingerEvenA bs) =
  TwoFingerOddE e (as <> bs)

appendEvenEOddE :: TwoFingerEvenE e a -> TwoFingerOddE e a -> TwoFingerOddE e a
appendEvenEOddE EmptyEvenE m = m
appendEvenEOddE (TwoFingerEvenE e as a) (TwoFingerOddE e' bs) =
  TwoFingerOddE e (as <> ((a, e') Seq.<| bs))
