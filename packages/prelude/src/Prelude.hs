{-# LANGUAGE RankNTypes #-}
module Prelude
  ( module Export
  , mtimesSafe
  , LText, SText, TBuilder
  , hush
  , (<&>)
  , (<>~)
  , FilePathComponent
  , validationToExcept, exceptToValidation, failure
  )
  where

--TODO: consider eliminating mappend as an export, in favour of (<>)? Slightly awkward since we need it visible when we define new Monoid instances (until GHC 8.4). So maybe stick on the 'do later' pile...
-- base imports for re-export
import Control.Applicative as Export
  (Applicative, pure, (<*>), (*>))
import Control.Category as Export
  (Category (id, (.)), (>>>))
import Control.Exception as Export
  (Exception)
import Control.Monad as Export
  (Monad, (=<<), (>>=), when, unless, ap, (<=<), void, guard)
import Control.Monad.Fail as Export
  (fail)
import Control.Monad.IO.Class as Export
  (liftIO)
--TODO: see below: same logic applies to ditching bifoldr
import Data.Bifoldable as Export
  (Bifoldable (bifoldMap), bitraverse_, bifold, biList, bifoldr, biall)
import Data.Bifunctor as Export
  (Bifunctor (bimap), first, second)
import Data.Bifunctor.Flip as Export
  (Flip (Flip), runFlip)
import Data.Bitraversable as Export
  (Bitraversable (bitraverse), bimapDefault, bifoldMapDefault)
import Data.Bool as Export
  (Bool (True, False), not, (||), (&&), otherwise)
import Data.Char as Export
  (Char, isSpace, toUpper, isAlphaNum)
import Data.Either as Export
  (Either (Left, Right), either, partitionEithers)
import Data.Eq as Export
  (Eq, (==), (/=))
--TODO: do we actually get anything out of Foldable if we use foldr/foldl'? That is, we might as well go through toList for those, right?
--TODO: elem is a potential performance landmine: if a container places more restrictions on its elements and can use those for retrieval, the (Eq a) constraint forces it to go linear. (e.g., Data.Set.member is O(log n).)
import Data.Foldable as Export
  ( Foldable (foldMap), toList, foldr, traverse_, sequence_, foldl', fold
  , for_, any, all, null, elem, find
  )
import Data.Function as Export
  (($), const, flip, (&), fix)
import Data.Functor as Export
  (Functor (fmap), (<$>))
import Data.Functor.Compose as Export
  (Compose (Compose), getCompose)
import Data.Functor.Const as Export
  (Const (Const), getConst)
import Data.Functor.Identity as Export
  (runIdentity, Identity (Identity))
import Data.Int as Export
  (Int)
import Data.List as Export
  (iterate)
import Data.List.NonEmpty as Export
  (NonEmpty ((:|)), unzip)
import Data.Maybe as Export
  (Maybe (Just, Nothing), maybe, fromMaybe, mapMaybe, catMaybes, isNothing)
import Data.Monoid as Export
  (Monoid (mempty, mappend), Endo (Endo), appEndo, Dual (Dual), getDual)
import Data.Ord as Export
  (Ord, (>=), (<=), (>), (<), min, max)
--TODO: get rid of Option once Maybe's Monoid instance changes
import Data.Semigroup as Export
  ( Semigroup ((<>)), All (All), getAll, Any (Any), getAny, Option (Option), option
  , First (First), getFirst, Last (Last), getLast
  )
import Data.Traversable as Export
  (Traversable, traverse, for, sequence, foldMapDefault, fmapDefault)
import Data.Type.Equality as Export
  ((:~:) (Refl))
import Data.Tuple as Export
  (uncurry, swap, fst, snd)
import Data.Void as Export
  (Void)
import Data.Word as Export
  (Word, Word8, Word64)
--TODO: is there really no other place but GHC.* to get these from? Should probably file an upstream bug...
import GHC.Base as Export
  (($!))
import GHC.Err as Export
  (error)
import GHC.Generics as Export
  (Generic)
import GHC.Num as Export
  ((*), (+), (-), Integer)
import GHC.Real as Export
  (Integral)
import Numeric.Natural as Export
  (Natural)
import System.Exit as Export
  (exitFailure)
import System.IO as Export
  (IO, FilePath, stdin, stdout, stderr, withFile, IOMode (ReadMode, WriteMode, AppendMode, ReadWriteMode))
import Text.Show as Export
  (Show, show)

-- deepseq imports for re-export
import Control.DeepSeq as Export
  (NFData)

-- filepath imports for re-export
--TODO: consider dropping the String-based ones?
import System.FilePath.Posix as Export
  (addExtension, dropExtension, (</>))

-- transformers imports for re-export
import Control.Monad.Trans.Accum as Export
  (runAccum, runAccumT, evalAccum, evalAccumT, AccumT, Accum)
import Control.Monad.Trans.Except as Export
  (withExceptT, runExceptT, runExcept, except, Except, ExceptT (ExceptT), mapExceptT)
import Control.Monad.Trans.Class as Export
  (lift)
import Control.Monad.Trans.State as Export
  (runStateT, execStateT, evalStateT, runState, StateT (StateT), State, state)
import Control.Monad.Trans.Writer as Export
  ( execWriter, execWriterT, runWriter, runWriterT, Writer, WriterT (WriterT)
  , writer
  )
import Data.Functor.Reverse as Export
  (Reverse (Reverse), getReverse)

-- containers imports for re-export
import Data.Map as Export
  (Map)
import Data.Set as Export
  (Set)
import Data.Sequence as Export
  (Seq)

-- semigroupoids imports for re-export
import Data.Functor.Apply as Export
  (Apply ((<.>)), MaybeApply (MaybeApply), runMaybeApply, WrappedApplicative (WrapApplicative), unwrapApplicative)
import Data.Semigroup.Bifoldable as Export
  (Bifoldable1 (bifoldMap1))
import Data.Semigroup.Bitraversable as Export
  (Bitraversable1 (bitraverse1), bifoldMap1Default)
import Data.Semigroup.Foldable as Export
  (Foldable1 (foldMap1, toNonEmpty))
import Data.Semigroup.Traversable as Export
  (Traversable1 (traverse1), foldMap1Default)

-- dlist imports for re-export
import Data.DList as Export
  (DList)

-- dlist-nonempty imports for re-export
import Data.DList.NonEmpty as Export
  (NonEmptyDList)

-- microens imports for re-export
import Lens.Micro as Export
  (Lens', set, over)
import Lens.Micro.Extras as Export
  (view)

-- profunctors imports for re-export
import Data.Profunctor as Export
  (Profunctor, dimap)

-- either imports for re-export
import Data.Either.Validation as Export
  (Validation, validationToEither, eitherToValidation)

-- Q4C12 packages for re-export
import Q4C12.FoldableUtils as Export
  ( intercalate0, intercalateMap0, biintercalateMap0
  , foldMapM, foldSequence, bifoldMapM
  , prependsMap, prepends, appendsMap, appends
  , unfoldr'
  )
import Q4C12.HList as Export
  (HSum (HSumHere, HSumThere), absurdHSum, eliminateHSum, partitionHSum, _HSumHere, _HSumThere, eitherSum, HProd (HProdCons, HProdNil), headL, tailL, unitProd, singleProd, doubleProd, dropUnit, HProdList (HProdListCons, HProdListNil))
import Q4C12.TwoFinger as Export
  (TwoFingerOddA, TwoFingerOddE, TwoFingerEvenA, TwoFingerEvenE)
import Q4C12.Optic as Export
  (Iso, Iso', from, iso, Prism, Prism', APrism, APrism', preview, matching, review, withPrism)

--Imports for local use.
import Data.Semigroup (mtimesDefault)
import Data.String (String)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import GHC.Real (fromIntegral)

type SText = ST.Text
type LText = LT.Text
type TBuilder = LTB.Builder

--TODO: get rid of all the local definitions that aren't purely for compatibility.

mtimesSafe :: (Monoid a) => Natural -> a -> a
mtimesSafe = mtimesDefault . (fromIntegral :: Natural -> Integer)

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

--TODO: this is in base in 8.4, so remove this defn then.
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>

--TODO: get this operator into microlens
(<>~) :: (Semigroup a) => Lens' s a -> a -> s -> s
l <>~ a = over l (<> a)

type FilePathComponent = String

validationToExcept :: (Applicative f) => Validation e a -> ExceptT e f a
validationToExcept = mapExceptT (pure . runIdentity) . except . validationToEither

exceptToValidation :: Except e a -> Validation e a
exceptToValidation = eitherToValidation . runExcept

failure :: e -> Validation e a
failure = eitherToValidation . Left
