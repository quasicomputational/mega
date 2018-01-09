{-# OPTIONS_HADDOCK not-home #-}
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
import Data.Bifoldable (Bifoldable (bifoldMap), biall)
import Data.Bitraversable
  (Bitraversable (bitraverse), bifoldMapDefault, bimapDefault)
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
import Data.Maybe (isNothing)
import Data.Semigroup (Semigroup ((<>)))
import Data.Semigroup.Bifoldable (Bifoldable1 (bifoldMap1))
import Data.Semigroup.Bitraversable
  (Bitraversable1 (bitraverse1), bifoldMap1Default)
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Semigroup.Traversable
  (Traversable1 (traverse1), foldMap1Default)
import Data.Stream.Infinite
  (Stream ((:>)))
import qualified Data.Stream.Infinite as Stream
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

-- * Types, EqN?\/ShowN?\/(Bi)Functor\/Foldable1?\/Traversable1?
-- instances, and odd traversals.

data Digit e a
  = One e
  | Two e a e
  | Three e a e a e
  | Four e a e a e a e
  deriving (Functor, Foldable, Traversable, Generic)

instance (NFData e, NFData a) => NFData (Digit e a)

instance Bifunctor Digit where
  bimap = bimapDefault

instance Bifoldable Digit where
  bifoldMap = bifoldMapDefault

instance Bifoldable1 Digit where
  bifoldMap1 = bifoldMap1Default

instance Bitraversable Digit where
  bitraverse = bitraverseDefault

instance Bitraversable1 Digit where
  bitraverse1 f _ (One e) = One <$> f e
  bitraverse1 f g (Two e1 a1 e2) = Two <$> f e1 <.> g a1 <.> f e2
  bitraverse1 f g (Three e1 a1 e2 a2 e3) =
    Three <$> f e1 <.> g a1 <.> f e2 <.> g a2 <.> f e3
  bitraverse1 f g (Four e1 a1 e2 a2 e3 a3 e4) =
    Four <$> f e1 <.> g a1 <.> f e2 <.> g a2 <.> f e3 <.> g a3 <.> f e4

data Node e a = Node2 e a e | Node3 e a e a e
  deriving (Functor, Foldable, Traversable, Eq, Generic)

instance (NFData e, NFData a) => NFData (Node e a)

instance Foldable1 (Node e) where
  foldMap1 = foldMap1Default

instance Traversable1 (Node e) where
  traverse1 f (Node2 e1 a1 e2) = Node2 e1 <$> f a1 <.*> pure e2
  traverse1 f (Node3 e1 a1 e2 a2 e3) =
    Node3 e1 <$> f a1 <.*> pure e2 <.> f a2 <.*> pure e3

instance Bifunctor Node where
  bimap = bimapDefault

instance Bifoldable Node where
  bifoldMap = bifoldMapDefault

instance Bifoldable1 Node where
  bifoldMap1 = bifoldMap1Default

instance Bitraversable Node where
  bitraverse = bitraverseDefault

instance Bitraversable1 Node where
  bitraverse1 f g (Node2 e1 a1 e2) = Node2 <$> f e1 <.> g a1 <.> f e2
  bitraverse1 f g (Node3 e1 a1 e2 a2 e3) =
    Node3 <$> f e1 <.> g a1 <.> f e2 <.> g a2 <.> f e3

-- | Isomorphic to @a, (e, a)*@
data TwoFingerOddA e a
  = EmptyOddA a
  | SingleOddA a e a
  | DeepOddA a !(Digit e a) (TwoFingerOddA (Node e a) a) !(Digit e a) a
  deriving (Generic)

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
  liftEq2 f g as bs = case alignLeftOddAOddA as bs of
    (aligned, rest) ->
      biall (uncurry f) (uncurry g) aligned && noMore rest
    where
      noMore :: Either (TwoFingerEvenE a b) (TwoFingerEvenE c d) -> Bool
      noMore = either (isNothing . unconsEvenE) (isNothing . unconsEvenE)

instance (Eq e) => Eq1 (TwoFingerOddA e) where
  liftEq = liftEq2 (==)

instance (Eq e, Eq a) => Eq (TwoFingerOddA e a) where
  (==) = eq2

instance (NFData e, NFData a) => NFData (TwoFingerOddA e a)

--TODO: If we had 'type>', we could document the lensiness directly.
--See https://github.com/sol/doctest/issues/153
-- | Access the first @a@ of a @'TwoFingerOddA' e a@. \(O(1)\). This
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
  traverse1 f (EmptyOddA a) = EmptyOddA <$> f a
  traverse1 f (SingleOddA a1 e1 a2) = SingleOddA <$> f a1 <.*> pure e1 <.> f a2
  traverse1 f (DeepOddA a0 pr m sf a1) = DeepOddA
     <$> f a0
    <.*> traverse (MaybeApply . Left . f) pr
     <.> bitraverse1 (traverse1 f) f m
    <.*> traverse (MaybeApply . Left . f) sf
     <.> f a1

instance Bifunctor TwoFingerOddA where
  bimap = bimapDefault

instance Bifoldable TwoFingerOddA where
  bifoldMap = bifoldMapDefault

instance Bifoldable1 TwoFingerOddA where
  bifoldMap1 = bifoldMap1Default

instance Bitraversable TwoFingerOddA where
  bitraverse = bitraverseDefault

instance Bitraversable1 TwoFingerOddA where
  bitraverse1 _ g (EmptyOddA a) = EmptyOddA <$> g a
  bitraverse1 f g (SingleOddA a1 e1 a2) = SingleOddA <$> g a1 <.> f e1 <.> g a2
  bitraverse1 f g (DeepOddA a0 pr m sf a1) = DeepOddA
    <$> g a0
    <.> bitraverse1 f g pr
    <.> bitraverse1 (bitraverse1 f g) g m
    <.> bitraverse1 f g sf
    <.> g a1

-- | Isomorphic to @e, (a, e)*@
data TwoFingerOddE e a
  = SingleOddE e
  | DeepOddE !(Digit e a) (TwoFingerOddA (Node e a) a) !(Digit e a)
  deriving (Generic)

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
  liftEq2 f g as bs = case alignLeftOddEOddE as bs of
    (aligned, rest) -> biall (uncurry f) (uncurry g) aligned && noMore rest
    where
      noMore :: Either (TwoFingerEvenA a b) (TwoFingerEvenA c d) -> Bool
      noMore = either (isNothing . unconsEvenA) (isNothing . unconsEvenA)

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

instance Bitraversable TwoFingerOddE where
  bitraverse f _ (SingleOddE e) = SingleOddE <$> f e
  bitraverse f g (DeepOddE pr m sf) = DeepOddE <$> bitraverse f g pr <*> bitraverse (bitraverse f g) g m <*> bitraverse f g sf

instance (NFData e, NFData a) => NFData (TwoFingerOddE e a)

--TODO: cleaner to offer TwoFingerEvenE1, without EmptyL?
-- | Isomorphic to @(e, a)*@
data TwoFingerEvenE e a
  = EmptyEvenE
  | SingleEvenE e a
  | DeepEvenE !(Digit e a) (TwoFingerOddA (Node e a) a) !(Digit e a) a
  deriving (Generic)

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
  liftEq2 f g as bs = case alignLeftEvenEEvenE as bs of
    (aligned, rest) -> biall (uncurry f) (uncurry g) aligned && noMore rest
    where
      noMore :: Either (TwoFingerEvenE a b) (TwoFingerEvenE c d) -> Bool
      noMore = either (isNothing . unconsEvenE) (isNothing . unconsEvenE)

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
  bitraverse f g (SingleEvenE e a) = SingleEvenE <$> f e <*> g a
  bitraverse f g (DeepEvenE pr m sf a) = DeepEvenE
    <$> bitraverse f g pr
    <*> bitraverse (bitraverse f g) g m
    <*> bitraverse f g sf
    <*> g a

-- | Isomorphic to @(a, e)*@
data TwoFingerEvenA e a
  = EmptyEvenA
  | SingleEvenA a e
  | DeepEvenA a !(Digit e a) (TwoFingerOddA (Node e a) a) !(Digit e a)
  deriving (Generic)

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
  liftEq2 f g as bs = case alignLeftEvenAEvenA as bs of
    (aligned, rest) -> biall (uncurry f) (uncurry g) aligned && noMore rest
    where
      noMore :: Either (TwoFingerEvenA a b) (TwoFingerEvenA c d) -> Bool
      noMore = either (isNothing . unconsEvenA) (isNothing . unconsEvenA)

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
  bitraverse _ _ EmptyEvenA = pure EmptyEvenA
  bitraverse f g (SingleEvenA a e) = SingleEvenA <$> g a <*> f e
  bitraverse f g (DeepEvenA a pr m sf) = DeepEvenA
    <$> g a
    <*> bitraverse f g pr
    <*> bitraverse (bitraverse f g) g m
    <*> bitraverse f g sf

-- * Digit operations.

digitToTree :: Digit e a -> TwoFingerOddE e a
digitToTree (One e) = SingleOddE e
digitToTree (Two e1 a1 e2) = DeepOddE (One e1) (EmptyOddA a1) (One e2)
digitToTree (Three e1 a1 e2 a2 e3) =
  DeepOddE (Two e1 a1 e2) (EmptyOddA a2) (One e3)
digitToTree (Four e1 a1 e2 a2 e3 a3 e4) =
  DeepOddE (Two e1 a1 e2) (EmptyOddA a2) (Two e3 a3 e4)

digitCons :: e -> a -> Digit e a -> Either (Digit e a, a, Node e a) (Digit e a)
digitCons e1 a1 (One e2) = Right $ Two e1 a1 e2
digitCons e1 a1 (Two e2 a2 e3) = Right $ Three e1 a1 e2 a2 e3
digitCons e1 a1 (Three e2 a2 e3 a3 e4) = Right $ Four e1 a1 e2 a2 e3 a3 e4
digitCons e1 a1 (Four e2 a2 e3 a3 e4 a4 e5) =
  Left (Two e1 a1 e2, a2, Node3 e3 a3 e4 a4 e5)

digitSnoc :: Digit e a -> a -> e -> Either (Node e a, a, Digit e a) (Digit e a)
digitSnoc (One e1) a1 e2 = Right $ Two e1 a1 e2
digitSnoc (Two e1 a1 e2) a2 e3 = Right $ Three e1 a1 e2 a2 e3
digitSnoc (Three e1 a1 e2 a2 e3) a3 e4 = Right $ Four e1 a1 e2 a2 e3 a3 e4
digitSnoc (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 =
  Left (Node3 e1 a1 e2 a2 e3, a3, Two e4 a4 e5)

digitUncons :: Digit e a -> (e, Maybe (a, Digit e a))
digitUncons (One e1) = (e1, Nothing)
digitUncons (Two e1 a1 e2) = (e1, Just (a1, One e2))
digitUncons (Three e1 a1 e2 a2 e3) = (e1, Just (a1, Two e2 a2 e3))
digitUncons (Four e1 a1 e2 a2 e3 a3 e4) =
  (e1, Just (a1, Three e2 a2 e3 a3 e4))

digitUnsnoc :: Digit e a -> (Maybe (Digit e a, a), e)
digitUnsnoc (One e1) = (Nothing, e1)
digitUnsnoc (Two e1 a1 e2) = (Just (One e1, a1), e2)
digitUnsnoc (Three e1 a1 e2 a2 e3) = (Just (Two e1 a1 e2, a2), e3)
digitUnsnoc (Four e1 a1 e2 a2 e3 a3 e4) =
  (Just (Three e1 a1 e2 a2 e3, a3), e4)

-- * Node operations.
nodeToDigit :: Node e a -> Digit e a
nodeToDigit (Node2 e1 a1 e2) = Two e1 a1 e2
nodeToDigit (Node3 e1 a1 e2 a2 e3) = Three e1 a1 e2 a2 e3

-- * Tree rotations
rotl :: TwoFingerOddA (Node e a) a -> Digit e a -> TwoFingerEvenA e a
rotl m sf = case unconsOddA m of
  Left a -> halfconsOddE a $ digitToTree sf
  Right ((a, e), m') -> DeepEvenA a (nodeToDigit e) m' sf

rotr :: Digit e a -> TwoFingerOddA (Node e a) a -> TwoFingerEvenE e a
rotr pr m = case unsnocOddA m of
  Left a -> halfsnocOddE (digitToTree pr) a
  Right (m', (e, a)) -> DeepEvenE pr m' (nodeToDigit e) a

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

-- | \(O(\log n)\) worst case. Inverse: 'halfunconsEvenE'
halfconsOddA :: e -> TwoFingerOddA e a -> TwoFingerEvenE e a
halfconsOddA e (EmptyOddA a) = SingleEvenE e a
halfconsOddA e (SingleOddA a1 e1 a2) =
  DeepEvenE (One e) (EmptyOddA a1) (One e1) a2
halfconsOddA e (DeepOddA a0 pr m sf a1) = case digitCons e a0 pr of
  Right pr' -> DeepEvenE pr' m sf a1
  Left (pr', a', node) -> DeepEvenE pr' (consOddA a' node m) sf a1

-- | \(O(\log n)\) worst case. Inverse: 'halfunsnocEvenA'
halfsnocOddA :: TwoFingerOddA e a -> e -> TwoFingerEvenA e a
halfsnocOddA (EmptyOddA a) e = SingleEvenA a e
halfsnocOddA (SingleOddA a e1 a1) e2 =
  DeepEvenA a (One e1) (EmptyOddA a1) (One e2)
halfsnocOddA (DeepOddA a0 pr m sf a1) e = case digitSnoc sf a1 e of
  Right sf' -> DeepEvenA a0 pr m sf'
  Left (node, a', sf') -> DeepEvenA a0 pr (snocOddA m node a') sf'

-- | \(O(1)\) worst case. Inverse: 'halfconsEvenE'
halfunconsOddA :: TwoFingerOddA e a -> (a, TwoFingerEvenE e a)
halfunconsOddA (EmptyOddA a) = (a, EmptyEvenE)
halfunconsOddA (SingleOddA a e1 a1) = (a, SingleEvenE e1 a1)
halfunconsOddA (DeepOddA a0 pr m sf a1) = (a0, DeepEvenE pr m sf a1)

-- | \(O(1)\) worst case. Inverse: 'halfsnocOddA'
halfunsnocOddA :: TwoFingerOddA e a -> (TwoFingerEvenA e a, a)
halfunsnocOddA (EmptyOddA a) = (EmptyEvenA, a)
halfunsnocOddA (SingleOddA a1 e1 a2) = (SingleEvenA a1 e1, a2)
halfunsnocOddA (DeepOddA a0 pr m sf a1) = (DeepEvenA a0 pr m sf, a1)

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

-- | \(O(1)\) worst case. Inverse: 'halfunconsEvenA'
halfconsOddE :: a -> TwoFingerOddE e a -> TwoFingerEvenA e a
halfconsOddE a (SingleOddE e) = SingleEvenA a e
halfconsOddE a (DeepOddE pr m sf) = DeepEvenA a pr m sf

-- | \(O(1)\) worst case. Inverse: 'halfunsnocEvenE'
halfsnocOddE :: TwoFingerOddE e a -> a -> TwoFingerEvenE e a
halfsnocOddE (SingleOddE e) a = SingleEvenE e a
halfsnocOddE (DeepOddE pr m sf) a = DeepEvenE pr m sf a

-- | \(O(\log n)\) worst case. Inverse: 'halfconsEvenA'
halfunconsOddE :: TwoFingerOddE e a -> (e, TwoFingerEvenA e a)
halfunconsOddE (SingleOddE e) = (e, EmptyEvenA)
halfunconsOddE (DeepOddE pr m sf) = case digitUncons pr of
  (e, Nothing) -> (e, rotl m sf)
  (e, Just (a, pr')) -> (e, DeepEvenA a pr' m sf)

-- | \(O(\log n)\) worst case. Inverse: 'halfsnocEvenE'
halfunsnocOddE :: TwoFingerOddE e a -> (TwoFingerEvenE e a, e)
halfunsnocOddE (SingleOddE e) = (EmptyEvenE, e)
halfunsnocOddE (DeepOddE pr m sf) = case digitUnsnoc sf of
  (Nothing, e) -> (rotr pr m, e)
  (Just (sf', a), e) -> (DeepEvenE pr m sf' a, e)

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

-- | \(O(1)\) worst case. Inverse: 'halfunconsOddA'
halfconsEvenE :: a -> TwoFingerEvenE e a -> TwoFingerOddA e a
halfconsEvenE a EmptyEvenE = EmptyOddA a
halfconsEvenE a0 (SingleEvenE e1 a1) = SingleOddA a0 e1 a1
halfconsEvenE a0 (DeepEvenE pr m sf a1) = DeepOddA a0 pr m sf a1

-- | \(O(\log n)\) worst case. Inverse: 'halfunsnocOddE'.
halfsnocEvenE :: TwoFingerEvenE e a -> e -> TwoFingerOddE e a
halfsnocEvenE EmptyEvenE e = SingleOddE e
halfsnocEvenE (SingleEvenE e1 a1) e2 =
  DeepOddE (One e1) (EmptyOddA a1) (One e2)
halfsnocEvenE (DeepEvenE pr m sf a) e = case digitSnoc sf a e of
  Right sf' -> DeepOddE pr m sf'
  Left (node, a', sf') -> DeepOddE pr (snocOddA m node a') sf'

-- | \(O(\log n)\) worst case. Inverse: 'halfconsOddA'.
halfunconsEvenE :: TwoFingerEvenE e a -> Maybe (e, TwoFingerOddA e a)
halfunconsEvenE EmptyEvenE = Nothing
halfunconsEvenE (SingleEvenE e a) = Just (e, EmptyOddA a)
halfunconsEvenE (DeepEvenE pr m sf a1) = Just $ case digitUncons pr of
  (e, Nothing) -> (e, halfsnocEvenA (rotl m sf) a1)
  (e, Just (a0, pr')) -> (e, DeepOddA a0 pr' m sf a1)

-- | \(O(1)\) worst case. Inverse: 'halfsnocOddE'.
halfunsnocEvenE :: TwoFingerEvenE e a -> Maybe (TwoFingerOddE e a, a)
halfunsnocEvenE EmptyEvenE = Nothing
halfunsnocEvenE (SingleEvenE e a) = Just (SingleOddE e, a)
halfunsnocEvenE (DeepEvenE pr m sf a) = Just (DeepOddE pr m sf, a)

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

-- | \(O(\log n)\) worst case. Inverse: 'halfunconsOddE'.
halfconsEvenA :: e -> TwoFingerEvenA e a -> TwoFingerOddE e a
halfconsEvenA e EmptyEvenA = SingleOddE e
halfconsEvenA e1 (SingleEvenA a1 e2) =
  DeepOddE (One e1) (EmptyOddA a1) (One e2)
halfconsEvenA e (DeepEvenA a pr m sf) = case digitCons e a pr of
  Right pr' -> DeepOddE pr' m sf
  Left (pr', a', node) -> DeepOddE pr' (consOddA a' node m) sf

-- | \(O(1)\) worst case. Inverse: 'halfunsnocOddA'.
halfsnocEvenA :: TwoFingerEvenA e a -> a -> TwoFingerOddA e a
halfsnocEvenA EmptyEvenA a = EmptyOddA a
halfsnocEvenA (SingleEvenA a1 e1) a2 = SingleOddA a1 e1 a2
halfsnocEvenA (DeepEvenA a0 pr m sf) a = DeepOddA a0 pr m sf a

-- | \(O(1)\) worst case. Inverse: 'halfconsOddE'.
halfunconsEvenA :: TwoFingerEvenA e a -> Maybe (a, TwoFingerOddE e a)
halfunconsEvenA EmptyEvenA = Nothing
halfunconsEvenA (SingleEvenA a e) = Just (a, SingleOddE e)
halfunconsEvenA (DeepEvenA a pr m sf) = Just (a, DeepOddE pr m sf)

-- | \(O(\log n)\) worst case. Inverse: 'halfsnocOddA'.
halfunsnocEvenA :: TwoFingerEvenA e a -> Maybe (TwoFingerOddA e a, e)
halfunsnocEvenA EmptyEvenA = Nothing
halfunsnocEvenA (SingleEvenA a e) = Just (EmptyOddA a, e)
halfunsnocEvenA (DeepEvenA a1 pr m sf) = case digitUnsnoc sf of
  (Nothing, e) -> Just (halfconsEvenE a1 (rotr pr m), e)
  (Just (sf', a2), e) -> Just (DeepOddA a1 pr m sf' a2, e)

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
singletonOddA = EmptyOddA

-- | Surrounds the argument with 'mempty'.
--
-- >>> unitOddA 3 :: TwoFingerOddA Int String
-- consOddA "" 3 (singletonOddA "")
unitOddA :: (Monoid a, Semigroup a) => e -> TwoFingerOddA e a
unitOddA a = consOddA mempty a mempty

-- |
-- >>> onlyOddA (singletonOddA "Hello!")
-- Just "Hello!"
-- >>> onlyOddA (consOddA True 3 $ singletonOddA False)
-- Nothing
onlyOddA :: TwoFingerOddA e a -> Maybe a
onlyOddA (EmptyOddA a) = Just a
onlyOddA _ = Nothing

-- |
-- >>> interleavingOddA "sep" (3 :| [4, 5])
-- consOddA 3 "sep" (consOddA 4 "sep" (singletonOddA 5))
interleavingOddA :: e -> NonEmpty a -> TwoFingerOddA e a
interleavingOddA sep (a :| as) =
  foldl' (flip snocOddA sep) (singletonOddA a) as

-- * Construction of TwoFingerOddE
singletonOddE :: e -> TwoFingerOddE e a
singletonOddE = SingleOddE

-- * Concatenation of TwoFingerOddA.

instance (Semigroup a) => Semigroup (TwoFingerOddA e a) where
  (<>) = appendOddA0

instance (Monoid a, Semigroup a) => Monoid (TwoFingerOddA e a) where
  mempty = singletonOddA mempty
  mappend = (<>)

appendOddA0
  :: (Semigroup a)
  => TwoFingerOddA e a
  -> TwoFingerOddA e a
  -> TwoFingerOddA e a
appendOddA0 (EmptyOddA a) (halfunconsOddA -> (a', m)) =
  halfconsEvenE (a <> a') m
appendOddA0 (SingleOddA a1 e1 a) (halfunconsOddA -> (a', m)) =
  consOddA a1 e1 $ halfconsEvenE (a <> a') m
appendOddA0 (halfunsnocOddA -> (m, a)) (EmptyOddA a') =
  halfsnocEvenA m (a <> a')
appendOddA0 (halfunsnocOddA -> (m, a)) (SingleOddA a' a1 e1) =
  snocOddA (halfsnocEvenA m (a <> a')) a1 e1
appendOddA0 (DeepOddA aa1 pr1 m1 sf1 az1) (DeepOddA aa2 pr2 m2 sf2 az2) =
  DeepOddA aa1 pr1 (addDigits0 m1 sf1 (az1 <> aa2) pr2 m2) sf2 az2

addDigits0
  :: TwoFingerOddA (Node e a) a
  -> Digit e a -> a -> Digit e a
  -> TwoFingerOddA (Node e a) a
  -> TwoFingerOddA (Node e a) a
addDigits0 m1 (One e1) a1 (One e2) m2 =
  appendOddA1 m1 (Node2 e1 a1 e2) m2
addDigits0 m1 (One e1) a1 (Two e2 a2 e3) m2 =
  appendOddA1 m1 (Node3 e1 a1 e2 a2 e3) m2
addDigits0 m1 (One e1) a1 (Three e2 a2 e3 a3 e4) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node2 e3 a3 e4) m2
addDigits0 m1 (One e1) a1 (Four e2 a2 e3 a3 e4 a4 e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits0 m1 (Two e1 a1 e2) a2 (One e3) m2 =
  appendOddA1 m1 (Node3 e1 a1 e2 a2 e3) m2
addDigits0 m1 (Two e1 a1 e2) a2 (Two e3 a3 e4) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node2 e3 a3 e4) m2
addDigits0 m1 (Two e1 a1 e2) a2 (Three e3 a3 e4 a4 e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits0 m1 (Two e1 a1 e2) a2 (Four e3 a3 e4 a4 e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits0 m1 (Three e1 a1 e2 a2 e3) a3 (One e4) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node2 e3 a3 e4) m2
addDigits0 m1 (Three e1 a1 e2 a2 e3) a3 (Two e4 a4 e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits0 m1 (Three e1 a1 e2 a2 e3) a3 (Three e4 a4 e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits0 m1 (Three e1 a1 e2 a2 e3) a3 (Four e4 a4 e5 a5 e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits0 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 (One e5) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) m2
addDigits0 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 (Two e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits0 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 (Three e5 a5 e6 a6 e7) m2 =
  appendOddA3 m1 (Node2 e1 a1 e2) a2 (Node2 e3 a3 e4) a4
    (Node3 e5 a5 e6 a6 e7) m2
addDigits0 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 (Four e5 a5 e6 a6 e7 a7 e8) m2 =
  appendOddA3 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2

appendOddA1 :: TwoFingerOddA e a -> e -> TwoFingerOddA e a -> TwoFingerOddA e a
appendOddA1 (EmptyOddA a) e m = consOddA a e m
appendOddA1 (SingleOddA a1 e1 a2) e2 m = consOddA a1 e1 $ consOddA a2 e2 m
appendOddA1 m e (EmptyOddA a) = snocOddA m e a
appendOddA1 m e1 (SingleOddA a1 e2 a2) = snocOddA (snocOddA m e1 a1) e2 a2
appendOddA1 (DeepOddA a0 pr1 m1 sf1 a1) e (DeepOddA a2 pr2 m2 sf2 az) =
  DeepOddA a0 pr1 (addDigits1 m1 sf1 a1 e a2 pr2 m2) sf2 az

addDigits1
  :: TwoFingerOddA (Node e a) a
  -> Digit e a -> a -> e -> a -> Digit e a
  -> TwoFingerOddA (Node e a) a
  -> TwoFingerOddA (Node e a) a
addDigits1 m1 (One e1) a1 e2 a2 (One e3) m2 =
  appendOddA1 m1 (Node3 e1 a1 e2 a2 e3) m2
addDigits1 m1 (One e1) a1 e2 a2 (Two e3 a3 e4) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node2 e3 a3 e4) m2
addDigits1 m1 (One e1) a1 e2 a2 (Three e3 a3 e4 a4 e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits1 m1 (One e1) a1 e2 a2 (Four e3 a3 e4 a4 e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits1 m1 (Two e1 a1 e2) a2 e3 a3 (One e4) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node2 e3 a3 e4) m2
addDigits1 m1 (Two e1 a1 e2) a2 e3 a3 (Two e4 a4 e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits1 m1 (Two e1 a1 e2) a2 e3 a3 (Three e4 a4 e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits1 m1 (Two e1 a1 e2) a2 e3 a3 (Four e4 a4 e5 a5 e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits1 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 (One e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits1 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 (Two e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits1 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 (Three e5 a5 e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits1 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 (Four e5 a5 e6 a6 e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits1 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 (One e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits1 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 (Two e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits1 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 (Three e6 a6 e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits1 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5
  (Four e6 a6 e7 a7 e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2

appendOddA2
  :: TwoFingerOddA e a
  -> e -> a -> e
  -> TwoFingerOddA e a
  -> TwoFingerOddA e a
appendOddA2 (EmptyOddA a1) e1 a2 e2 m =
  consOddA a1 e1 $ consOddA a2 e2 m
appendOddA2 (SingleOddA a1 e1 a2) e2 a3 e3 m =
  consOddA a1 e1 $ consOddA a2 e2 $ consOddA a3 e3 m
appendOddA2 m e1 a1 e2 (EmptyOddA a2) =
  snocOddA (snocOddA m e1 a1) e2 a2
appendOddA2 m e1 a1 e2 (SingleOddA a2 e3 a3) =
  snocOddA (snocOddA (snocOddA m e1 a1) e2 a2) e3 a3
appendOddA2 (DeepOddA a0 pr1 m1 sf1 a1) e1 a2 e2 (DeepOddA a3 pr2 m2 sf2 az) =
  DeepOddA a0 pr1 (addDigits2 m1 sf1 a1 e1 a2 e2 a3 pr2 m2) sf2 az

addDigits2
  :: TwoFingerOddA (Node e a) a
  -> Digit e a -> a -> e -> a -> e -> a -> Digit e a
  -> TwoFingerOddA (Node e a) a
  -> TwoFingerOddA (Node e a) a
addDigits2 m1 (One e1) a1 e2 a2 e3 a3 (One e4) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node2 e3 a3 e4) m2
addDigits2 m1 (One e1) a1 e2 a2 e3 a3 (Two e4 a4 e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits2 m1 (One e1) a1 e2 a2 e3 a3 (Three e4 a4 e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits2 m1 (One e1) a1 e2 a2 e3 a3 (Four e4 a4 e5 a5 e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits2 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 (One e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits2 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 (Two e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits2 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 (Three e5 a5 e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits2 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 (Four e5 a5 e6 a6 e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits2 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 (One e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits2 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 (Two e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits2 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 (Three e6 a6 e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits2 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5
  (Four e6 a6 e7 a7 e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits2 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 (One e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits2 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 (Two e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits2 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6
  (Three e7 a7 e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits2 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6
  (Four e7 a7 e8 a8 e9 a9 e10) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node2 e9 a9 e10) m2

appendOddA3
  :: TwoFingerOddA e a
  -> e -> a -> e -> a -> e
  -> TwoFingerOddA e a
  -> TwoFingerOddA e a
appendOddA3 (EmptyOddA a1) e1 a2 e2 a3 e3 m =
  consOddA a1 e1 $ consOddA a2 e2 $ consOddA a3 e3 m
appendOddA3 m e1 a1 e2 a2 e3 (EmptyOddA a3) =
  snocOddA (snocOddA (snocOddA m e1 a1) e2 a2) e3 a3
appendOddA3 (SingleOddA a1 e1 a2) e2 a3 e3 a4 e4 m =
  consOddA a1 e1 $ consOddA a2 e2 $ consOddA a3 e3 $ consOddA a4 e4 m
appendOddA3 m e1 a1 e2 a2 e3 (SingleOddA a3 e4 a4) =
  snocOddA (snocOddA (snocOddA (snocOddA m e1 a1) e2 a2) e3 a3) e4 a4
appendOddA3 (DeepOddA a0 pr1 m1 sf1 a1) e1 a2 e2 a3 e3
  (DeepOddA a4 pr2 m2 sf2 az) =
    DeepOddA a0 pr1 (addDigits3 m1 sf1 a1 e1 a2 e2 a3 e3 a4 pr2 m2) sf2 az

addDigits3
  :: TwoFingerOddA (Node e a) a
  -> Digit e a -> a -> e -> a -> e -> a -> e -> a -> Digit e a
  -> TwoFingerOddA (Node e a) a
  -> TwoFingerOddA (Node e a) a
addDigits3 m1 (One e1) a1 e2 a2 e3 a3 e4 a4 (One e5) m2 =
  appendOddA2 m1 (Node2 e1 a1 e2) a2 (Node3 e3 a3 e4 a4 e5) m2
addDigits3 m1 (One e1) a1 e2 a2 e3 a3 e4 a4 (Two e5 a5 e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits3 m1 (One e1) a1 e2 a2 e3 a3 e4 a4 (Three e5 a5 e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits3 m1 (One e1) a1 e2 a2 e3 a3 e4 a4 (Four e5 a5 e6 a6 e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits3 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 e5 a5 (One e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits3 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 e5 a5 (Two e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits3 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 e5 a5 (Three e6 a6 e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits3 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 e5 a5
  (Four e6 a6 e7 a7 e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits3 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 e6 a6 (One e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits3 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 e6 a6 (Two e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits3 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 e6 a6
  (Three e7 a7 e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits3 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 e6 a6
  (Four e7 a7 e8 a8 e9 a9 e10) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node2 e9 a9 e10) m2
addDigits3 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 e7 a7 (One e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits3 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 e7 a7
  (Two e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits3 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 e7 a7
  (Three e8 a8 e9 a9 e10) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node2 e9 a9 e10) m2
addDigits3 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 e7 a7
  (Four e8 a8 e9 a9 e10 a10 e11) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node3 e9 a9 e10 a10 e11) m2

appendOddA4
  :: TwoFingerOddA e a
  -> e -> a -> e -> a -> e -> a -> e
  -> TwoFingerOddA e a
  -> TwoFingerOddA e a
appendOddA4 (EmptyOddA a1) e1 a2 e2 a3 e3 a4 e4 m =
  consOddA a1 e1 $ consOddA a2 e2 $ consOddA a3 e3 $ consOddA a4 e4 m
appendOddA4 m e1 a1 e2 a2 e3 a3 e4 (EmptyOddA a4) =
  snocOddA (snocOddA (snocOddA (snocOddA m e1 a1) e2 a2) e3 a3) e4 a4
appendOddA4 (SingleOddA a1 e1 a2) e2 a3 e3 a4 e4 a5 e5 m =
  consOddA a1 e1 $ consOddA a2 e2 $ consOddA a3 e3 $ consOddA a4 e4 $
    consOddA a5 e5 m
appendOddA4 m e1 a1 e2 a2 e3 a3 e4 (SingleOddA a4 e5 a5) =
  snocOddA (snocOddA (snocOddA (snocOddA (snocOddA m e1 a1) e2 a2) e3 a3) e4 a4) e5 a5
appendOddA4 (DeepOddA a0 pr1 m1 sf1 a1) e1 a2 e2 a3 e3 a4 e4
  (DeepOddA a5 pr2 m2 sf2 an) =
    DeepOddA a0 pr1 (addDigits4 m1 sf1 a1 e1 a2 e2 a3 e3 a4 e4 a5 pr2 m2) sf2 an

addDigits4
  :: TwoFingerOddA (Node e a) a
  -> Digit e a -> a -> e -> a -> e -> a -> e -> a -> e -> a -> Digit e a
  -> TwoFingerOddA (Node e a) a
  -> TwoFingerOddA (Node e a) a
addDigits4 m1 (One e1) a1 e2 a2 e3 a3 e4 a4 e5 a5 (One e6) m2 =
  appendOddA2 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) m2
addDigits4 m1 (One e1) a1 e2 a2 e3 a3 e4 a4 e5 a5 (Two e6 a6 e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits4 m1 (One e1) a1 e2 a2 e3 a3 e4 a4 e5 a5 (Three e6 a6 e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits4 m1 (One e1) a1 e2 a2 e3 a3 e4 a4 e5 a5
  (Four e6 a6 e7 a7 e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits4 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 e5 a5 e6 a6 (One e7) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node2 e6 a6 e7) m2
addDigits4 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 e5 a5 e6 a6 (Two e7 a7 e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits4 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 e5 a5 e6 a6
  (Three e7 a7 e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits4 m1 (Two e1 a1 e2) a2 e3 a3 e4 a4 e5 a5 e6 a6
  (Four e7 a7 e8 a8 e9 a9 e10) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node2 e9 a9 e10) m2
addDigits4 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 e6 a6 e7 a7 (One e8) m2 =
  appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node2 e4 a4 e5) a5
    (Node3 e6 a6 e7 a7 e8) m2
addDigits4 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 e6 a6 e7 a7
  (Two e8 a8 e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits4 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 e6 a6 e7 a7
  (Three e8 a8 e9 a9 e10) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node2 e9 a9 e10) m2
addDigits4 m1 (Three e1 a1 e2 a2 e3) a3 e4 a4 e5 a5 e6 a6 e7 a7
  (Four e8 a8 e9 a9 e10 a10 e11) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node3 e9 a9 e10 a10 e11) m2
addDigits4 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 e7 a7 e8 a8
  (One e9) m2 =
    appendOddA3 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) m2
addDigits4 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 e7 a7 e8 a8
  (Two e9 a9 e10) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node2 e9 a9 e10) m2
addDigits4 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 e7 a7 e8 a8
  (Three e9 a9 e10 a10 e11) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node2 e7 a7 e8) a8 (Node3 e9 a9 e10 a10 e11) m2
addDigits4 m1 (Four e1 a1 e2 a2 e3 a3 e4) a4 e5 a5 e6 a6 e7 a7 e8 a8
  (Four e9 a9 e10 a10 e11 a11 e12) m2 =
    appendOddA4 m1 (Node3 e1 a1 e2 a2 e3) a3 (Node3 e4 a4 e5 a5 e6) a6
      (Node3 e7 a7 e8 a8 e9) a9 (Node3 e10 a10 e11 a11 e12) m2

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
appendEvenE (SingleEvenE e a) m = consEvenE e a m
appendEvenE m (SingleEvenE e a) = snocEvenE m e a
appendEvenE (DeepEvenE pr1 m1 sf1 b1) (DeepEvenE pr2 m2 sf2 b2) =
  DeepEvenE pr1 (addDigits0 m1 sf1 b1 pr2 m2) sf2 b2

-- * Concatenation of TwoFingerEvenA.

instance Semigroup (TwoFingerEvenA e a) where
  (<>) = appendEvenA

instance Alt (TwoFingerEvenA e) where
  (<!>) = appendEvenA

instance Monoid (TwoFingerEvenA e a) where
  mempty = EmptyEvenA
  mappend = (<>)

instance Plus (TwoFingerEvenA e) where
  zero = EmptyEvenA

appendEvenA :: TwoFingerEvenA e a -> TwoFingerEvenA e a -> TwoFingerEvenA e a
appendEvenA EmptyEvenA m = m
appendEvenA m EmptyEvenA = m
appendEvenA (SingleEvenA a e) m = consEvenA a e m
appendEvenA m (SingleEvenA a e) = snocEvenA m a e
appendEvenA (DeepEvenA a1 pr1 m1 sf1) (DeepEvenA a2 pr2 m2 sf2) =
  DeepEvenA a1 pr1 (addDigits0 m1 sf1 a2 pr2 m2) sf2

-- * Monoid actions

appendOddAEvenE :: TwoFingerOddA e a -> TwoFingerEvenE e a -> TwoFingerOddA e a
appendOddAEvenE (EmptyOddA a) m = halfconsEvenE a m
appendOddAEvenE m EmptyEvenE = m
appendOddAEvenE (SingleOddA a1 e a2) m = consOddA a1 e $ halfconsEvenE a2 m
appendOddAEvenE m (SingleEvenE e a) = snocOddA m e a
appendOddAEvenE (DeepOddA a1 pr1 m1 sf1 a2) (DeepEvenE pr2 m2 sf2 a3) =
  DeepOddA a1 pr1 (addDigits0 m1 sf1 a2 pr2 m2) sf2 a3

appendEvenAOddA :: TwoFingerEvenA e a -> TwoFingerOddA e a -> TwoFingerOddA e a
appendEvenAOddA EmptyEvenA m = m
appendEvenAOddA m (EmptyOddA a) = halfsnocEvenA m a
appendEvenAOddA (SingleEvenA a e) m = consOddA a e m
appendEvenAOddA m (SingleOddA a1 e1 a2) = snocOddA (halfsnocEvenA m a1) e1 a2
appendEvenAOddA (DeepEvenA a1 pr1 m1 sf1) (DeepOddA a2 pr2 m2 sf2 b) =
  DeepOddA a1 pr1 (addDigits0 m1 sf1 a2 pr2 m2) sf2 b

appendOddAOddE :: TwoFingerOddA e a -> TwoFingerOddE e a -> TwoFingerEvenA e a
appendOddAOddE (EmptyOddA a) m = halfconsOddE a m
appendOddAOddE (SingleOddA a1 e a2) m = consEvenA a1 e $ halfconsOddE a2 m
appendOddAOddE m (SingleOddE e) = halfsnocOddA m e
appendOddAOddE (DeepOddA a1 pr1 m1 sf1 a2) (DeepOddE pr2 m2 sf2) =
  DeepEvenA a1 pr1 (addDigits0 m1 sf1 a2 pr2 m2) sf2

appendOddEOddA :: TwoFingerOddE e a -> TwoFingerOddA e a -> TwoFingerEvenE e a
appendOddEOddA m (EmptyOddA a) = halfsnocOddE m a
appendOddEOddA (SingleOddE e) m = halfconsOddA e m
appendOddEOddA m (SingleOddA a1 e a2) = snocEvenE (halfsnocOddE m a1) e a2
appendOddEOddA (DeepOddE pr1 m1 sf1) (DeepOddA a1 pr2 m2 sf2 a2) =
  DeepEvenE pr1 (addDigits0 m1 sf1 a1 pr2 m2) sf2 a2

appendOddEEvenA :: TwoFingerOddE e a -> TwoFingerEvenA e a -> TwoFingerOddE e a
appendOddEEvenA m EmptyEvenA = m
appendOddEEvenA (SingleOddE e) m = halfconsEvenA e m
appendOddEEvenA m (SingleEvenA a e) = snocOddE m a e
appendOddEEvenA (DeepOddE pr1 m1 sf1) (DeepEvenA a pr2 m2 sf2) =
  DeepOddE pr1 (addDigits0 m1 sf1 a pr2 m2) sf2

appendEvenEOddE :: TwoFingerEvenE e a -> TwoFingerOddE e a -> TwoFingerOddE e a
appendEvenEOddE EmptyEvenE m = m
appendEvenEOddE (SingleEvenE a e) m = consOddE a e m
appendEvenEOddE m (SingleOddE e) = halfsnocEvenE m e
appendEvenEOddE (DeepEvenE pr1 m1 sf1 a) (DeepOddE pr2 m2 sf2) =
  DeepOddE pr1 (addDigits0 m1 sf1 a pr2 m2) sf2

-- * Aligning/zipping.

-- | Align two 'TwoFingerOddA' sequences elementwise, and return the excess remainder.
--
-- >>> alignLeftOddAOddA (consOddA 'a' 1 $ consOddA 'b' 2 $ singletonOddA 'c') (consOddA "foo" 10 $ singletonOddA "bar")
-- (consOddA ('a',"foo") (1,10) (singletonOddA ('b',"bar")),Left (consEvenE 2 'c' mempty))
--
-- >>> alignLeftOddAOddA (consOddA 'a' 1 $ singletonOddA 'b') (consOddA "foo" 10 $ consOddA "bar" 20 $ singletonOddA "baz")
-- (consOddA ('a',"foo") (1,10) (singletonOddA ('b',"bar")),Right (consEvenE 20 "baz" mempty))
alignLeftOddAOddA :: TwoFingerOddA e a -> TwoFingerOddA e' a' -> (TwoFingerOddA (e, e') (a, a'), Either (TwoFingerEvenE e a) (TwoFingerEvenE e' a'))
alignLeftOddAOddA as (halfunsnocOddA -> (bs, a')) = case alignLeftOddAEvenA as bs of
  Left (aligned, halfunconsOddA -> (a, rest)) ->
    (halfsnocEvenA aligned (a, a'), Left rest)
  Right (aligned, rest) -> (aligned, Right $ halfsnocOddE rest a')

--TODO: if we had TwoFingerEvenE1, we could avoid the arbitrary Left/Right selection in the Left/Nothing case.
-- |
-- >>> alignLeftOddAEvenA (consOddA 'a' 1 $ consOddA 'b' 2 $ singletonOddA 'c') (consEvenA "foo" 10 mempty)
-- Left (consEvenA ('a',"foo") (1,10) mempty,consOddA 'b' 2 (singletonOddA 'c'))
--
-- >>> alignLeftOddAEvenA (consOddA 'a' 1 $ singletonOddA 'b') (consEvenA "foo" 10 $ consEvenA "bar" 20 $ consEvenA "baz" 30 mempty)
-- Right (consOddA ('a',"foo") (1,10) (singletonOddA ('b',"bar")),consOddE 20 "baz" (singletonOddE 30))
alignLeftOddAEvenA :: TwoFingerOddA e a -> TwoFingerEvenA e' a' -> Either (TwoFingerEvenA (e, e') (a, a'), TwoFingerOddA e a) (TwoFingerOddA (e, e') (a, a'), TwoFingerOddE e' a')
alignLeftOddAEvenA as bs = case (unconsOddA as, unconsEvenA bs) of
  (Right ((a, e), as'), Just ((a', e'), bs')) -> case alignLeftOddAEvenA as' bs' of
    Left (aligned, rest) -> Left (consEvenA (a, a') (e, e') aligned, rest)
    Right (aligned, rest) -> Right (consOddA (a, a') (e, e') aligned, rest)
  (_, Nothing) -> Left (mempty, as)
  (Left a, Just ((a', e'), bs')) -> Right (singletonOddA (a, a'), halfconsEvenA e' bs')

alignLeftEvenAEvenA :: TwoFingerEvenA e a -> TwoFingerEvenA e' a' -> (TwoFingerEvenA (e, e') (a, a'), Either (TwoFingerEvenA e a) (TwoFingerEvenA e' a'))
alignLeftEvenAEvenA as bs = case (unconsEvenA as, unconsEvenA bs) of
  (Just ((a, e), as'), Just ((a', e'), bs')) -> case alignLeftEvenAEvenA as' bs' of
    (aligned, rest) -> (consEvenA (a, a') (e, e') aligned, rest)
  (_, Nothing) -> (mempty, Left as)
  (Nothing, _) -> (mempty, Right bs)

-- |
-- >>> alignLeftOddEOddE (consOddE 'a' 1 $ consOddE 'b' 2 $ singletonOddE 'c') (consOddE "foo" 10 $ singletonOddE "bar")
-- (consOddE ('a',"foo") (1,10) (singletonOddE ('b',"bar")),Left (consEvenA 2 'c' mempty))
--
-- >>> alignLeftOddEOddE (consOddE 'a' 1 $ singletonOddE 'b') (consOddE "foo" 10 $ consOddE "bar" 20 $ singletonOddE "baz")
-- (consOddE ('a',"foo") (1,10) (singletonOddE ('b',"bar")),Right (consEvenA 20 "baz" mempty))
alignLeftOddEOddE :: TwoFingerOddE e a -> TwoFingerOddE e' a' -> (TwoFingerOddE (e, e') (a, a'), Either (TwoFingerEvenA e a) (TwoFingerEvenA e' a'))
alignLeftOddEOddE as (halfunsnocOddE -> (bs, e')) = case alignLeftOddEEvenE as bs of
  Left (aligned, halfunconsOddE -> (e, rest)) -> (halfsnocEvenE aligned (e, e'), Left rest)
  Right (aligned, rest) -> (aligned, Right $ halfsnocOddA rest e')

alignLeftOddEEvenE :: TwoFingerOddE e a -> TwoFingerEvenE e' a' -> Either (TwoFingerEvenE (e, e') (a, a'), TwoFingerOddE e a) (TwoFingerOddE (e, e') (a, a'), TwoFingerOddA e' a')
alignLeftOddEEvenE as bs = case (unconsOddE as, unconsEvenE bs) of
  (Right ((e, a), as'), Just ((e', a'), bs')) -> case alignLeftOddEEvenE as' bs' of
    Left (aligned, rest) -> Left (consEvenE (e, e') (a, a') aligned, rest)
    Right (aligned, rest) -> Right (consOddE (e, e') (a, a') aligned, rest)
  (_, Nothing) -> Left (mempty, as)
  (Left e, Just ((e', a'), bs')) -> Right (singletonOddE (e, e'), halfconsEvenE a' bs')

alignLeftEvenEEvenE :: TwoFingerEvenE e a -> TwoFingerEvenE e' a' -> (TwoFingerEvenE (e, e') (a, a'), Either (TwoFingerEvenE e a) (TwoFingerEvenE e' a'))
alignLeftEvenEEvenE as bs = case (unconsEvenE as, unconsEvenE bs) of
  (Just ((e, a), as'), Just ((e', a'), bs')) -> case alignLeftEvenEEvenE as' bs' of
    (aligned, rest) -> (consEvenE (e, e') (a, a') aligned, rest)
  (_, Nothing) -> (mempty, Left as)
  (Nothing, _) -> (mempty, Right bs)

-- * Creating infinite sequences.

--TODO: we can actually work with either finite or infinite sequences here, right? Oh, not quite: if both sides are finite, we'll have a parity mismatch, so that can't work even in theory. One side infinite and one side finite could be wonky: if we peer too deeply into the finite side, we'll bottom out. So maybe it makes sense for either both to be finite (with an extra a to balance), or both to be infinite; but, in the finite case, if the things are flagrantly different lengths, we'd be better off building with cons/snoc rather than structurally. On the other hand, it might be useful to be able to build a tree without committing to it being finite or infinite. The worry is the unexpected bottoming.

takeNodeLeft :: (Stream a -> es -> (e, Stream a, es)) -> Stream a -> es -> (Node e a, Stream a, es)
takeNodeLeft f as es =
  let (x, a :> as', es') = f as es
      (y, as'', es'') = f as' es'
  in (Node2 x a y, as'', es'')

takeNodeRight :: (es -> Stream a -> (e, es, Stream a)) -> es -> Stream a -> (Node e a, es, Stream a)
takeNodeRight f es as =
  let (x, es', a :> as') = f es as
      (y, es'', as'') = f es' as'
  in (Node2 y a x, es'', as'')

takeSingleNodeLeft :: Stream a -> Stream e -> (Node e a, Stream a, Stream e)
takeSingleNodeLeft = takeNodeLeft (\as (e :> es) -> (e, as, es))

takeSingleNodeRight :: Stream e -> Stream a -> (Node e a, Stream e, Stream a)
takeSingleNodeRight = takeNodeRight (\(e :> es) as -> (e, es, as))

infiniteOddA'
  :: (Stream a -> Stream e -> (Node e' a, Stream a, Stream e))
  -> (Stream e -> Stream a -> (Node e' a, Stream e, Stream a))
  -> Stream a -> Stream e
  -> Stream e -> Stream a
  -> TwoFingerOddA e' a
infiniteOddA' f g (a0 :> leftA) leftE rightE (an :> rightA) =
  let (prNode, leftA', leftE') = f leftA leftE
      (sfNode, rightE', rightA') = g rightE rightA
      inner = infiniteOddA' (takeNodeLeft f) (takeNodeRight g) leftA' leftE' rightE' rightA'
  in DeepOddA a0 (nodeToDigit prNode) inner (nodeToDigit sfNode) an

-- | Infinitely repeat the given @a@ and @e@.
repeatOddA :: a -> e -> TwoFingerOddA e a
repeatOddA a e = infiniteOddA (Stream.iterate id a) (Stream.iterate id e) (Stream.iterate id e) (Stream.iterate id a)

-- | From streams of leftward @a@, leftward @e@, rightward @e@ and
-- rightward @a@, build an infinite 'TwoFingerOddA'.
--
-- >>> let infinite = infiniteOddA (Stream.iterate (+ 1) 0) (Stream.iterate (+ 1) 10) (Stream.iterate (+ 1) 20) (Stream.iterate (+ 1) 30)
-- >>> take 5 $ unfoldr (hush . unconsOddA) infinite
-- [(0,10),(1,11),(2,12),(3,13),(4,14)]
-- >>> take 5 $ unfoldr (fmap swap . hush . unsnocOddA) infinite
-- [(20,30),(21,31),(22,32),(23,33),(24,34)]
infiniteOddA :: Stream a -> Stream e -> Stream e -> Stream a -> TwoFingerOddA e a
infiniteOddA = infiniteOddA' takeSingleNodeLeft takeSingleNodeRight

-- | Infinitely repeat the given @a@ and @e@.
repeatOddE :: e -> a -> TwoFingerOddE e a
repeatOddE e a = infiniteOddE (Stream.iterate id e) (Stream.iterate id a) (Stream.iterate id a) (Stream.iterate id e)

-- |
--
-- >>> let infinite = infiniteOddE (Stream.iterate (+ 1) 0) (Stream.iterate (+ 1) 10) (Stream.iterate (+ 1) 20) (Stream.iterate (+ 1) 30)
-- >>> take 5 $ unfoldr (hush . unconsOddE) infinite
-- [(0,10),(1,11),(2,12),(3,13),(4,14)]
-- >>> take 5 $ unfoldr (fmap swap . hush . unsnocOddE) infinite
-- [(20,30),(21,31),(22,32),(23,33),(24,34)]
infiniteOddE :: Stream e -> Stream a -> Stream a -> Stream e -> TwoFingerOddE e a
infiniteOddE leftE leftA rightA rightE =
  DeepOddE (nodeToDigit prNode) inner (nodeToDigit sfNode)
  where
    (prNode, leftE', leftA') = takeSingleNodeLeft leftA leftE
    (sfNode, rightA', rightE') = takeSingleNodeRight rightE rightA
    inner = infiniteOddA' (takeNodeLeft takeSingleNodeLeft) (takeNodeRight takeSingleNodeRight) leftE' leftA' rightA' rightE'

-- | Infinitely repeat the given @a@ and @e@.
repeatEvenA :: a -> e -> TwoFingerEvenA e a
repeatEvenA a e = infiniteEvenA (Stream.iterate id a) (Stream.iterate id e) (Stream.iterate id a) (Stream.iterate id e)

-- |
--
-- >>> let infinite = infiniteEvenA (Stream.iterate (+ 1) 0) (Stream.iterate (+ 1) 10) (Stream.iterate (+ 1) 20) (Stream.iterate (+ 1) 30)
-- >>> take 5 $ unfoldr unconsEvenA infinite
-- [(0,10),(1,11),(2,12),(3,13),(4,14)]
-- >>> take 5 $ unfoldr (fmap swap . unsnocEvenA) infinite
-- [(20,30),(21,31),(22,32),(23,33),(24,34)]
infiniteEvenA :: Stream a -> Stream e -> Stream a -> Stream e -> TwoFingerEvenA e a
infiniteEvenA (a :> leftA) leftE rightA rightE =
  halfconsOddE a $ infiniteOddE leftE leftA rightA rightE

repeatEvenE :: e -> a -> TwoFingerEvenE e a
repeatEvenE e a = infiniteEvenE (Stream.iterate id e) (Stream.iterate id a) (Stream.iterate id e) (Stream.iterate id a)

-- |
--
-- >>> let infinite = infiniteEvenE (Stream.iterate (+ 1) 0) (Stream.iterate (+ 1) 10) (Stream.iterate (+ 1) 20) (Stream.iterate (+ 1) 30)
-- >>> take 5 $ unfoldr unconsEvenE infinite
-- [(0,10),(1,11),(2,12),(3,13),(4,14)]
-- >>> take 5 $ unfoldr (fmap swap . unsnocEvenE) infinite
-- [(20,30),(21,31),(22,32),(23,33),(24,34)]
infiniteEvenE :: Stream e -> Stream a -> Stream e -> Stream a -> TwoFingerEvenE e a
infiniteEvenE leftE leftA rightE (a :> rightA) =
  halfsnocOddE (infiniteOddE leftE leftA rightA rightE) a
