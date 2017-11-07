{-# LANGUAGE RankNTypes #-}
module Q4C12.Optic
  ( Iso, Iso', AnIso, Exchange (Exchange), from, iso, withIso
  , Prism, Prism', APrism, APrism', Market (Market), prism, preview, matching, review, withPrism
  )
  where

import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.Profunctor (Profunctor (dimap), Choice (right'))

type Iso s t a b = forall f p . (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange fr to) = Exchange (fr . f) (g . to)

type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

from :: AnIso s t a b -> Iso b a t s
from i = withIso i (flip iso)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso fr to = dimap fr (fmap to)

withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso f k = case f $ Exchange id Identity of
  Exchange fr ito -> k fr (runIdentity . ito)

type Prism s t a b = forall f p . (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

data Market a b s t = Market (b -> t) (s -> Either t a)

instance Profunctor (Market a b) where
  dimap f g (Market fr to) = Market (g . fr) (first g . to . f)

instance Choice (Market a b) where
  right' (Market f g) = Market (Right . f) (either (Left . Left) (first Right . g))

type APrism s t a b = Market a b a (Identity b) -> Market a b s (Identity t)

type APrism' s a = APrism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism f g = dimap g (either pure $ fmap f) . right'

review :: APrism s t a b -> b -> t
review p = withPrism p $ \fr _ -> fr

preview :: APrism' s a -> s -> Maybe a
preview p = either (const Nothing) Just . matching p

matching :: APrism s t a b -> s -> Either t a
matching p = withPrism p $ \_ to -> to

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism f k = case f $ Market pure Right of
  Market fr to -> k (runIdentity . fr) (first runIdentity . to)
