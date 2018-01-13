module Q4C12.FoldableUtils
  ( -- * Intercalation
    intercalate0, intercalateMap0, biintercalateMap0,
    -- * Foldable applicatives
    foldMapM, foldSequence, bifoldMapM,
    -- * Semigroup actions
    prependsMap, prepends, appendsMap, appends,
    -- * Unfolding
    unfoldr',
  )
  where

import Data.Bifunctor (first)
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Functor.Reverse (Reverse (Reverse))
import Data.Semigroup (Semigroup ((<>)), Dual (Dual), getDual, Endo (Endo), appEndo, Option (Option))

-- $setup
-- >>> import Data.Char (toUpper)
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Monoid (Sum (Sum), getSum)
-- >>> import Data.Semigroup (Last (Last), getLast, Max (Max), getMax)

newtype Joined a = Joined { runJoined :: a -> a }

instance (Semigroup a) => Semigroup (Joined a) where
  Joined f <> Joined g = Joined $ \a -> f a <> a <> g a

-- | Like 'Data.List.intercalate', but for any 'Monoid' and 'Foldable'.
--
-- See 'Data.Semigroup.Foldable.intercalate1' in the @semigroupoids@ for a related function that operates on 'Semigroup' and 'Data.Semigroup.Foldable.Foldable1'.
--
-- >>> getSum $ intercalate0 (Sum 100) ((Sum 3) :| [Sum 5, Sum 9])
-- 217
intercalate0 :: (Monoid a, Semigroup a, Foldable f) => a -> f a -> a
intercalate0 a = intercalateMap0 a id

--TODO: this might be a candidate for upstreaming in base? Maybe even going directly in as a Foldable method, since it's 100% conceivable that a more efficient implementation exists for a specific type. (Especially since you can derive a good default foldMap from it...)
--TODO: in the other direction, a lensy intercalateOf would be pretty handy to have (and would admit an easy intercalateMap implementation).
-- | Apply a function to each element of the sequence, insert a separator between each element, and then concatenate them all together. @'intercalateMap0' sep f@ is like @'intercalate0' sep '.' 'fmap' f@, but without the need for a 'Functor' constraint.
--
-- See 'Data.Semigroup.Foldable.intercalateMap1' in the @semigroupoids@ for a related function that operates on 'Semigroup' and 'Data.Semigroup.Foldable.Foldable1'.
--
-- >>> intercalateMap0 "-" (map toUpper) ["example", "string"]
-- "EXAMPLE-STRING"
intercalateMap0
  :: (Monoid m, Semigroup m, Foldable f)
  => m
  -> (a -> m)
  -> f a
  -> m
intercalateMap0 a f
  = foldMap (flip runJoined a)
  . foldMap (Option . Just . Joined . const . f)

-- | 'intercalateMap0', extended to 'Bifoldable'.
--
-- >>> getSum $ biintercalateMap0 (Sum 100) (Sum . (* 3)) (Sum . (* 4)) (20, 1)
-- 164
biintercalateMap0
  :: (Monoid m, Semigroup m, Bifoldable p)
  => m
  -> (a -> m)
  -> (b -> m)
  -> p a b
  -> m
biintercalateMap0 a f g
  = foldMap (flip runJoined a)
  . bifoldMap (Option . Just . Joined . const . f) (Option . Just . Joined . const . g)

--TODO: this could be a more general utility...
newtype MonoidA f m = MonoidA { getMonoidA :: f m }

--TODO: this only requires Apply, not Applicative
instance (Applicative f, Semigroup m) => Semigroup (MonoidA f m) where
  MonoidA a <> MonoidA b = MonoidA $ (<>) <$> a <*> b

instance (Applicative f, Semigroup m, Monoid m) => Monoid (MonoidA f m) where
  mempty = MonoidA $ pure mempty
  mappend = (<>)

-- | Applicative 'foldMap'.
--
-- >>> foldMapM (\str -> putStrLn str *> pure (map toUpper str)) ["words", "more words"]
-- words
-- more words
-- "WORDSMORE WORDS"
foldMapM
  :: (Semigroup m, Monoid m, Foldable t, Applicative f)
  => (a -> f m)
  -> t a
  -> f m
foldMapM f = getMonoidA . foldMap (MonoidA . f)

-- | Applicative 'Data.Foldable.fold'.
--
-- >>> foldSequence [putStrLn "foo" *> pure [1], putStrLn "bar" *> pure [2]]
-- foo
-- bar
-- [1,2]
foldSequence
  :: (Semigroup m, Monoid m, Foldable t, Applicative f)
  => t (f m)
  -> f m
foldSequence = foldMapM id

-- | 'foldMapM', extended to 'Bifoldable'. An applicative version of 'bifoldMap'.
bifoldMapM
  :: (Semigroup m, Monoid m, Bifoldable t, Applicative f)
  => (a -> f m)
  -> (b -> f m)
  -> t a b
  -> f m
bifoldMapM f g = getMonoidA . bifoldMap (MonoidA . f) (MonoidA . g)

--TODO: more candidates for upstreaming... somewhere?
-- | Left semigroup action.
--
-- >>> prependsMap (fmap show) [1 :| [2, 3], 4 :| []] ("foo" :| ["bar"])
-- "1" :| ["2","3","4","foo","bar"]
prependsMap :: (Semigroup a, Foldable f) => (b -> a) -> f b -> a -> a
prependsMap f = appEndo . foldMap (\b -> Endo (f b <>))

-- | Left semigroup action.
--
-- >>> getMax $ prepends [Max 9, Max 23, Max 7] (Max 12)
-- 23
prepends :: (Semigroup a, Foldable f) => f a -> a -> a
prepends = prependsMap id

-- | Right semigroup action.
appendsMap :: (Semigroup a, Foldable f) => (b -> a) -> a -> f b -> a
appendsMap f a bs = getDual $ prependsMap (Dual . f) (Reverse bs) (Dual a)

-- | Right semigroup action.
--
-- >>> getLast $ appends (Last 1) [Last 2, Last 3]
-- 3
appends :: (Semigroup a, Foldable f) => a -> f a -> a
appends = appendsMap id

--TODO: Upstreamable? Lists are the correct type here. Must also look at how base's unfoldr is implemented, to steal its tricks...
-- | Like 'Data.List.unfold', but allowing you to return a residual value as well.
--
-- >>> unfoldr' (\n -> if n < 10 then Left n else Right (show n, n - 10)) 22
-- (["22","12"],2)
unfoldr' :: (b -> Either r (a, b)) -> b -> ([a], r)
unfoldr' f b = case f b of
  Left r -> ([], r)
  Right (a, b') -> first (a :) $ unfoldr' f b'
