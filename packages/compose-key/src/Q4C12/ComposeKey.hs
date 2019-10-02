module Q4C12.ComposeKey
  ( composeKey
  , decomposeKey
  , mapOfSetsToSet
  )
  where

import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Map.Internal as Map.Internal
import Data.Set ( Set )

-- * Composition

-- TODO: upstream?

-- Note that this is slightly lossy - the inner map can be empty, but that and the no-key-at-all case are collapsed together.
composeKey :: Map a ( Map b c ) -> Map ( a, b ) c
composeKey = \case
  Map.Internal.Tip -> Map.Internal.Tip
  Map.Internal.Bin _ k v l r
    -> Map.Internal.link2 ( composeKey l )
     $ Map.Internal.link2 ( Map.mapKeysMonotonic ( (,) k ) v )
     $ composeKey r

-- * Decomposition

data SplayedNonEmptyMap k v
  = SNEOne k v
  -- Invariant: the first k must be less than all keys of the map, and
  -- the second k must be greater than all keys of the map.
  | SNEMany k v ( Map k v ) k v

type SplayedMap k v = Maybe ( SplayedNonEmptyMap k v )

unsplay :: SplayedNonEmptyMap k v -> Map k v
unsplay = \case
  SNEOne k v -> Map.singleton k v
  SNEMany k0 v0 inner k1 v1
    -> Map.Internal.link k0 v0 Map.empty
     $ Map.Internal.link k1 v1 inner
     $ Map.empty

-- Precondition: all keys of the left argument must be less than or equal to all keys of the right argument.
link2S :: ( Eq k ) => ( v -> v -> v ) -> SplayedNonEmptyMap k v -> SplayedNonEmptyMap k v -> SplayedNonEmptyMap k v
link2S combine = curry $ \case
  ( SNEOne k0 v0, SNEOne k1 v1 ) ->
    if k0 == k1
    then SNEOne k0 ( combine v0 v1 )
    else SNEMany k0 v0 Map.empty k1 v1
  ( SNEOne k0 v0, SNEMany k1 v1 inner k2 v2 ) ->
    if k0 == k1
    then SNEMany k0 ( combine v0 v1 ) inner k2 v2
    else SNEMany k0 v0 ( Map.Internal.link k1 v1 Map.empty inner ) k2 v2
  ( SNEMany k0 v0 inner k1 v1, SNEOne k2 v2 ) ->
    if k1 == k2
    then SNEMany k0 v0 inner k1 ( combine v1 v2 )
    else SNEMany k0 v0 ( Map.Internal.link k1 v1 inner Map.empty ) k2 v2
  ( SNEMany k0 v0 left k1 v1, SNEMany k2 v2 right k3 v3 ) ->
    let
      inner =
        if k1 == k2
        then Map.Internal.link k1 ( combine v1 v2 ) left right
        else Map.Internal.link k1 v1 left $ Map.Internal.link k2 v2 Map.empty right
    in
      SNEMany k0 v0 inner k3 v3

-- Precondition: all keys of the left argument must be less than or equal to all keys of the right argument.
link2SLeft :: ( Eq k ) => ( v -> v -> v ) -> SplayedNonEmptyMap k v -> SplayedMap k v -> SplayedNonEmptyMap k v
link2SLeft combine left right = case right of
  Nothing -> left
  Just right' -> link2S combine left right'

-- Precondition: all keys of the left argument must be less than or equal to all keys of the right argument.
link2SRight :: ( Eq k ) => ( v -> v -> v ) -> SplayedMap k v -> SplayedNonEmptyMap k v -> SplayedNonEmptyMap k v
link2SRight combine left right = case left of
  Nothing -> right
  Just left' -> link2S combine left' right

decomposeKeyS :: ( Eq a ) => Map ( a, b ) c -> SplayedMap a ( Map b c )
decomposeKeyS = \case
  Map.Internal.Tip -> Nothing
  Map.Internal.Bin _ ( a, b ) c l r
    -> Just
     $ link2SRight Map.Internal.link2 ( decomposeKeyS l )
     $ link2SLeft Map.Internal.link2 ( SNEOne a ( Map.singleton b c ) )
     $ decomposeKeyS r

-- The return type here is slightly too loose: the inner 'Map' is never empty.
-- TODO: use non-empty maps/sets once https://github.com/haskell/containers/pull/616 lands.
decomposeKey :: ( Eq a ) => Map ( a, b ) c -> Map a ( Map b c )
decomposeKey = maybe Map.empty unsplay . decomposeKeyS

-- * Utilities

mapOfSetsToSet :: Map k ( Set a ) -> Set ( k, a )
mapOfSetsToSet = Map.keysSet . composeKey . fmap ( Map.fromSet ( const () ) )
