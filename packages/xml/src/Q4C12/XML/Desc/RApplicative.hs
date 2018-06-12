{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Q4C12.XML.Desc.RApplicative
  ( RFunctor (rfmap)
  , RPlus (rempty, rplus), rchoice
  , RPlusApplyR (ActionR, rconsR), rright
  , RAlternative (rnil, rmany, rsome), rcons
  , roptional
  )
  where

--TODO: We have an Apply-a-like. What's the Traverse1-a-like??

-- Laws:
--   1. rfmap id a === a
--   2. rfmap (f . g) a === rfmap f . rfmap g $ a
--TODO: hang on, is that in the right order? Not rfmap g . rfmap f?
class RFunctor f where
  rfmap :: Iso' a b -> f a -> f b

--Laws:
--  1. rplus rempty a ~~~ a
--  2. rplus a (rplus rempty b) ~~~ rplus a b
--  3. rplus (rplus a b) c ~~~ rplus a (rplus b c)
--  4. rfmap (bimapping f g) (rplus a b) ~~~ rplus (rfmap f a) (rfmap g b)
--TODO: that 4th law doesn't quite look right to me. Do we also need something about swap, or do we get that for free?
--TODO: describe what we do about overlapping patterns.
class (RFunctor f) => RPlus f where
  rempty :: f (HSum '[])
  rplus :: f a -> f (HSum as) -> f (HSum (a ': as))

--TODO: define what happens here wrt ordering and overlap?
rchoice :: (RPlus f) => Iso' s (Either t a) -> f a -> f t -> f s
rchoice p fa ft = rfmap (from eitherSum . from p) $ rplus ft $ rplus fa rempty

--TODO: the RAlternative constraint is actually way too strong: we only need the equivalent of RApply (which is equivalent to saying rconsR = rconsL). (But remember that RAlternative gives us more laws than just RApply!)
--Also TODO: we don't include RApplyL here, because we don't have any use for it, but that's another superclass RAlternative can have...
-- Laws:
--   1. rconsR a (rconsR b c) ~~~ rconsR (rcons a b) c
--   2. rconsR rnil a ~~~ a
--   3. rfmap (bimapping f g) (rconsR a b) ~~~ rconsR (rfmap f a) (rfmap g b)
--   4. rconsR (rplus a b) c ~~~ rplus (rconsR a c) (rconsR b c)
--   5. rconsR rempty a ~~~ rempty
--   6. rconsR a rempty ~~~ rempty
--TODO: skipped nodes in the hierarchy!
class (RAlternative (ActionR f), RPlus f) => RPlusApplyR f where
  type ActionR f :: Type -> Type
  rconsR :: ActionR f a -> f (HProd as) -> f (HProd (a ': as))

rcons :: (RAlternative f) => f a -> f (HProd as) -> f (HProd (a ': as))
rcons = rconsR

rright :: (RPlusApplyR f) => ActionR f () -> f a -> f a
rright a b = rfmap (dropUnit . from singleProd) $ rconsR a $
  rfmap singleProd b

--TODO: we could use XMLDesc.TH to define rmany, rsome and roptional, at a cost of a more complicated module structure.
--Laws:
--  1. rcons (rplus a b) c ~~~ rplus (rcons a c) (rcons b c)
--  2. rcons a rnil ~~~ a
--TODO: that's left distribution---what about right distribution??
--TODO: we have skipped RApplicative in the hierarchy, because our specific application doesn't need it. (Without skipping it, RAlternative would be a laws-only class, too.)
class (RPlusApplyR f, ActionR f ~ f) => RAlternative f where
  rnil :: f (HProd '[])

  rmany :: f a -> f [a]
  rmany prs = rfmap (iso f g) $
    rplus (rcons prs $ rcons (rmany prs) rnil) $ rplus rnil rempty
    where
      f :: HSum '[HProd '[a, [a]], HProd '[]] -> [a]
      f (HSumHere (HProdCons a (HProdCons as HProdNil))) = a : as
      f (HSumThere (HSumHere HProdNil)) = []
      f (HSumThere (HSumThere a)) = case a of {}
      g :: [a] -> HSum '[HProd '[a, [a]], HProd '[]]
      g [] = HSumThere $ HSumHere HProdNil
      g (a:as) = HSumHere $ HProdCons a $ HProdCons as HProdNil

  rsome :: f a -> f (NonEmpty a)
  rsome prs = rfmap (iso f g) $ rcons prs $ rcons (rmany prs) rnil
    where
      f :: HProd '[a, [a]] -> NonEmpty a
      f (HProdCons a (HProdCons as HProdNil)) = a :| as
      g :: NonEmpty a -> HProd '[a, [a]]
      g (a :| as) = HProdCons a $ HProdCons as HProdNil

roptional :: (RAlternative f) => f a -> f (Maybe a)
roptional prs = rfmap (iso f g) $ rplus prs $ rplus rnil rempty
  where
    f :: HSum '[a, HProd '[]] -> Maybe a
    f (HSumHere a) = Just a
    f (HSumThere (HSumHere HProdNil)) = Nothing
    f (HSumThere (HSumThere a)) = case a of {}
    g :: Maybe a -> HSum '[a, HProd '[]]
    g = maybe (HSumThere $ HSumHere HProdNil) HSumHere
