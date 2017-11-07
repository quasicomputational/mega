module Q4C12.Validate
  ( Validate (Validate), runValidate
  , validateToEither, validateToExcept, exceptToValidate
  , failure
  )
  where

import Control.Monad.Trans.Except (Except, ExceptT (ExceptT), runExcept)
import Data.Bifunctor (first)
import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.Apply (Apply ((<.>)), MaybeApply (MaybeApply), runMaybeApply)
import Data.Semigroup (Semigroup)

newtype Validate e a = Validate { runValidate :: MaybeApply (Const e) a }
  deriving (Functor)

--TODO: present these as Isos? The difficulty is validateToExcept's any-Applicative thing...
validateToEither :: Validate e a -> Either e a
validateToEither = first getConst . runMaybeApply . runValidate

validateToExcept :: (Applicative f) => Validate e a -> ExceptT e f a
validateToExcept = ExceptT . pure . validateToEither

exceptToValidate :: (Semigroup e) => Except e a -> Validate e a
exceptToValidate = either failure pure . runExcept

instance (Semigroup e) => Apply (Validate e) where
  (<.>) = (<*>)

instance (Semigroup e) => Applicative (Validate e) where
  pure = Validate . pure
  Validate f <*> Validate a = Validate (f <*> a)

failure :: e -> Validate e a
failure = Validate . MaybeApply . Left . Const
