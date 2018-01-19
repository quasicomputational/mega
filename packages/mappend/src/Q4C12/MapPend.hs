module Q4C12.MapPend
  ( MapPend (MapPend)
  , getMapPend
  , _MapPend
  , empty
  , singleton
  , at
  )
  where

import Control.Lens (Iso, iso)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup ((<>)))

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

--Note: can't use alterF because that's not in older containers, which are boot libraries with GHC 8.0 and earlier. TODO: switch to alterF once they're out of the support window.
at :: (Ord k, Functor f) => k -> (Maybe v -> f (Maybe v)) -> MapPend k v -> f (MapPend k v)
at = \k f (MapPend m) -> MapPend . g k m <$> f (Map.lookup k m)
  where
    g :: (Ord k) => k -> Map k v -> Maybe v -> Map k v
    g k m = maybe (Map.delete k m) (\a -> Map.insert k a m)
