module Q4C12.MapPend
  ( MapPend (MapPend)
  , getMapPend
  , _MapPend
  , empty
  , singleton
  , at
  )
  where

import qualified Data.Map as Map

newtype MapPend k v = MapPend { getMapPend :: Map k v }
  deriving (Functor, Foldable, Traversable)

instance (Semigroup v, Ord k) => Semigroup (MapPend k v) where
  MapPend a <> MapPend b = MapPend $ Map.unionWith (<>) a b

instance (Semigroup v, Ord k) => Monoid (MapPend k v) where
  mempty = MapPend mempty
  mappend = (<>)

_MapPend :: Iso (MapPend k v) (MapPend k' v') (Map k v) (Map k' v')
_MapPend = iso getMapPend MapPend

empty :: MapPend k v
empty = MapPend Map.empty

singleton :: k -> v -> MapPend k v
singleton k = MapPend . Map.singleton k

at :: (Ord k, Functor f) => k -> (Maybe v -> f (Maybe v)) -> MapPend k v -> f (MapPend k v)
at k = _MapPend . flip Map.alterF k
