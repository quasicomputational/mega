{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Q4C12.HList
  ( HSum (HSumHere, HSumThere)
  , absurdHSum, eliminateHSum, partitionHSum
  , eitherSum
  , _HSumHere, _HSumThere
  , HProd (HProdNil, HProdCons)
  , headL, tailL
  , doubleProd, singleProd, unitProd, dropUnit
  , hprodding, hheading, htailing
  , distributeHead, distributeTail
  , HProdList (HProdListNil, HProdListCons), EmptyHProdList
  )
  where

import Data.Bifunctor (bimap)
import Control.Lens (over, Iso, Iso', AnIso, withIso, iso, Prism, prism)

--TODO: consider replacing this module with generics-sop or similar.

--TODO: think about syntactic sugar for these, esp. low-order ones.
data HSum :: [*] -> * where
  HSumHere :: a -> HSum (a ': as)
  HSumThere :: HSum as -> HSum (a ': as)

_HSumHere :: Prism (HSum (a ': as)) (HSum (a' ': as)) a a'
_HSumHere = prism HSumHere (eliminateHSum Right $ Left . HSumThere)

_HSumThere :: Prism (HSum (a ': as)) (HSum (a ': as')) (HSum as) (HSum as')
_HSumThere = prism HSumThere (eliminateHSum (Left . HSumHere) Right)

eliminateHSum :: (a -> r) -> (HSum as -> r) -> HSum (a ': as) -> r
eliminateHSum f _ (HSumHere a) = f a
eliminateHSum _ g (HSumThere as) = g as

absurdHSum :: HSum '[] -> a
absurdHSum a = case a of {}

--TODO: privileges lists. This can be very sensibly defined for maps as well, and a whole lot more.
partitionHSum :: (EmptyHProdList as) => [HSum as] -> HProdList as
partitionHSum [] = emptyHProdList
partitionHSum (a : as) = go a $ partitionHSum as
  where
    go :: HSum as -> HProdList as -> HProdList as
    go (HSumHere x) (HProdListCons xs rest) = HProdListCons (x : xs) rest
    go (HSumThere y) (HProdListCons xs rest) = HProdListCons xs (go y rest)

eitherSum :: Iso (Either a b) (Either a' b') (HSum '[a, b]) (HSum '[a', b'])
eitherSum = iso (either HSumHere (HSumThere . HSumHere)) (eliminateHSum Left $ eliminateHSum Right $ absurdHSum)

--TODO: an unzip operation? [HProd as] -> HProdList as?
data HProd :: [*] -> * where
  HProdNil :: HProd '[]
  HProdCons :: a -> HProd as -> HProd (a ': as)

doubleProd :: Iso (a, b) (a', b') (HProd '[a, b]) (HProd '[a', b'])
doubleProd = iso f g
  where
    f :: (a, b) -> HProd '[a, b]
    f (a, b) = HProdCons a $ HProdCons b HProdNil
    g :: HProd '[a, b] -> (a, b)
    g (HProdCons a (HProdCons b HProdNil)) = (a, b)

singleProd :: Iso a a' (HProd '[a]) (HProd '[a'])
singleProd = iso f g
  where
    f :: a -> HProd '[a]
    f a = HProdCons a HProdNil
    g :: HProd '[a] -> a
    g (HProdCons a HProdNil) = a

unitProd :: Iso' () (HProd '[])
unitProd = iso (\() -> HProdNil) (\HProdNil -> ())

dropUnit :: Iso (HProd (() ': as)) (HProd (() ': as')) (HProd as) (HProd as')
dropUnit = iso f g
  where
    f :: HProd (() ': as) -> HProd as
    f (HProdCons () as) = as
    g :: HProd as -> HProd (() ': as)
    g = HProdCons ()

headL :: (Functor f) => (a -> f b) -> HProd (a ': xs) -> f (HProd (b ': xs))
headL f (HProdCons a xs) = flip HProdCons xs <$> f a

tailL :: (Functor f) => (HProd as -> f (HProd bs)) -> HProd (x ': as) -> f (HProd (x ': bs))
tailL f (HProdCons x as) = HProdCons x <$> f as

hheading :: AnIso s t a b -> Iso (HProd (s ': xs)) (HProd (t ': ys)) (HProd (a ': xs)) (HProd (b ': ys))
hheading i = hprodding i id

htailing :: AnIso (HProd ss) (HProd ts) (HProd as) (HProd bs) -> Iso (HProd (x ': ss)) (HProd (y ': ts)) (HProd (x ': as)) (HProd (y ': bs))
htailing = hprodding id

hprodding :: AnIso s t a b -> AnIso (HProd ss) (HProd ts) (HProd as) (HProd bs) -> Iso (HProd (s ': ss)) (HProd (t ': ts)) (HProd (a ': as)) (HProd (b ': bs))
hprodding h t = withIso h $ \hfr hto -> withIso t $ \tfr tto ->
  iso (\(HProdCons s ss) -> HProdCons (hfr s) (tfr ss)) (\(HProdCons b bs) -> HProdCons (hto b) (tto bs))

distributeHead :: Iso s s' (Either t a) (Either t' a') -> Iso (HProd (s ': xs)) (HProd (s' ': ys)) (Either (HProd (t ': xs)) (HProd (a ': xs))) (Either (HProd (t' ': ys)) (HProd (a' ': ys)))
distributeHead i = withIso i $ \fr to -> iso
  (\(HProdCons s xs) -> bimap (flip HProdCons xs) (flip HProdCons xs) (fr s))
  (either (over headL $ to . Left) (over headL $ to . Right))

distributeTail :: Iso (HProd ss) (HProd ss') (Either (HProd ts) (HProd as)) (Either (HProd ts') (HProd as')) -> Iso (HProd (x ': ss)) (HProd (y ': ss')) (Either (HProd (x ': ts)) (HProd (x ': as))) (Either (HProd (y ': ts')) (HProd (y ': as')))
distributeTail i = withIso i $ \fr to -> iso
  (\(HProdCons x ss) -> bimap (HProdCons x) (HProdCons x) (fr ss))
  (either (over tailL $ to . Left) (over tailL $ to . Right))

--TODO: We could unify this with HProd, as HProdF :: (k -> *) -> [k] -> *. I tried that on a branch and GHC got very, very slow though, so...
data HProdList :: [*] -> * where
  HProdListNil :: HProdList '[]
  HProdListCons :: [a] -> HProdList as -> HProdList (a ': as)

class EmptyHProdList (as :: [*]) where
  emptyHProdList :: HProdList as

instance EmptyHProdList '[] where
  emptyHProdList = HProdListNil

instance (EmptyHProdList as) => EmptyHProdList (a ': as) where
  emptyHProdList = HProdListCons [] emptyHProdList
