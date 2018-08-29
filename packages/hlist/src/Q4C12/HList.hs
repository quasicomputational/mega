{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Q4C12.HList
  ( HSum (HSumHere, HSumThere)
  , absurdHSum, eliminateHSum
  , eitherSum
  , _HSumHere, _HSumThere
  , HProd (HProdNil, HProdCons)
  , headL, tailL
  , doubleProd, singleProd, unitProd, dropUnit
  )
  where

import Data.Bifunctor (bimap)
import Data.Kind (Type)
import Control.Lens (over, Iso, Iso', AnIso, withIso, iso, Prism, prism)

--TODO: consider replacing this module with generics-sop or similar.

--TODO: think about syntactic sugar for these, esp. low-order ones.
data HSum :: [Type] -> Type where
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

eitherSum :: Iso (Either a b) (Either a' b') (HSum '[a, b]) (HSum '[a', b'])
eitherSum = iso (either HSumHere (HSumThere . HSumHere)) (eliminateHSum Left $ eliminateHSum Right absurdHSum)

--TODO: an unzip operation? [HProd as] -> HProdList as?
data HProd :: [Type] -> Type where
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
