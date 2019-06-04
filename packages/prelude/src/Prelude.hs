{-# LANGUAGE RankNTypes #-}
module Prelude
  ( module Export
  , mtimesSafe
  , LText, SText, TBuilder
  , LByteString, SByteString, BSBuilder
  , hush
  , hoistMaybe
  , FilePathComponent
  , validationToExcept, exceptToValidation, failure
  , truncateInteger
  , divModInteger
  , fromIntegerClip
  )
  where

-- base imports for re-export
import Control.Applicative as Export
  (Applicative, Alternative, pure, (<*>), (*>), (<*), liftA2, many, empty, (<|>))
import Control.Category as Export
  (Category (id, (.)), (>>>))
import Control.Exception as Export
  (Exception)
import Control.Monad as Export
  (Monad, (=<<), (>>=), when, unless, ap, (<=<), void, guard, forever)
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
  (Char, isSpace, toUpper, isAlphaNum, isAscii)
import Data.Data as Export
  ( Data
  )
import Data.Either as Export
  (Either (Left, Right), either, partitionEithers)
import Data.Eq as Export
  (Eq, (==), (/=))
import Data.Fixed as Export
  ( mod'
  )
--TODO: do we actually get anything out of Foldable if we use foldr/foldl'? That is, we might as well go through toList for those, right?
--Note: elem is a potential performance landmine: if a container places more restrictions on its elements and can use those for retrieval, the (Eq a) constraint forces it to go linear. (e.g., Data.Set.member is O(log n).) Hence, we don't want to export it generally.
import Data.Foldable as Export
  ( Foldable (foldMap), toList, foldr, traverse_, sequence_, foldl', fold
  , for_, any, all, null, find, length
  )
import Data.Function as Export
  (($), const, flip, (&), fix, on)
import Data.Functor as Export
  (Functor (fmap), (<$>), (<$), (<&>))
import Data.Functor.Compose as Export
  (Compose (Compose), getCompose)
import Data.Functor.Const as Export
  (Const (Const), getConst)
import Data.Functor.Contravariant as Export
  ( Contravariant ( contramap )
  )
import Data.Functor.Identity as Export
  (runIdentity, Identity (Identity))
import Data.Int as Export
  (Int)
import Data.Kind as Export
  (Type)
import Data.List as Export
  (iterate, unfoldr)
import Data.List.NonEmpty as Export
  (NonEmpty ((:|)), unzip)
import Data.Maybe as Export
  (Maybe (Just, Nothing), maybe, fromMaybe, mapMaybe, catMaybes, isNothing, isJust)
import Data.Monoid as Export
  (Monoid (mempty), Endo (Endo), appEndo, Dual (Dual), getDual)
import Data.Ord as Export
  (Ord, (>=), (<=), (>), (<), min, max, Ordering (LT, EQ, GT), Down (Down))
import Data.Proxy as Export
  ( Proxy ( Proxy )
  , asProxyTypeOf
  )
import Data.Semigroup as Export
  ( Semigroup ((<>)), All (All), getAll, Any (Any), getAny
  , First (First), getFirst, Last (Last), getLast
  )
import Data.Traversable as Export
  (Traversable, traverse, for, sequenceA, foldMapDefault, fmapDefault)
import Data.Type.Equality as Export
  ((:~:) (Refl))
import Data.Tuple as Export
  (uncurry, swap, fst, snd)
import Data.Void as Export
  (Void, absurd)
import Data.Word as Export
  (Word, Word8, Word64)
--TODO: is there really no other place but GHC.* to get these from? Should probably file an upstream bug...
import GHC.Base as Export
  (($!))
import GHC.Enum as Export
  ( Bounded
  , maxBound
  , minBound
  , fromEnum
  )
import GHC.Err as Export
  (error)
import GHC.Float as Export
  (Double)
import GHC.Generics as Export
  (Generic)
import GHC.Num as Export
  ((*), (+), (-), Integer, negate, subtract)
import GHC.Real as Export
  (Integral, (/), quot, div, rem, mod, quotRem, divMod, toInteger)
import Numeric.Natural as Export
  (Natural)
import System.Exit as Export
  (exitFailure, ExitCode (ExitFailure, ExitSuccess))
import System.IO as Export
  (IO, FilePath, stdin, stdout, stderr, withFile, IOMode (ReadMode, WriteMode, AppendMode, ReadWriteMode), hSetBuffering, BufferMode (LineBuffering, NoBuffering, BlockBuffering), hFlush)
import System.IO.Error as Export
  ( isDoesNotExistError
  )
import Text.Show as Export
  (Show, show)

-- deepseq imports for re-export
import Control.DeepSeq as Export
  (NFData)

-- filepath imports for re-export
--TODO: consider dropping the String-based ones?
import System.FilePath.Posix as Export
  (addExtension, dropExtension, hasExtension, isExtensionOf, stripExtension, takeBaseName, (</>))

-- transformers imports for re-export
import Control.Monad.Trans.Accum as Export
  (runAccum, runAccumT, evalAccum, evalAccumT, AccumT, Accum)
import Control.Monad.Trans.Except as Export
  (withExceptT, runExceptT, runExcept, except, Except, ExceptT (ExceptT), mapExceptT)
import Control.Monad.Trans.Class as Export
  (lift)
import Control.Monad.Trans.Maybe as Export
  ( MaybeT, runMaybeT )
import Control.Monad.Trans.State as Export
  (runStateT, execStateT, evalStateT, runState, StateT (StateT), State, state, evalState, mapStateT)
import Data.Functor.Reverse as Export
  (Reverse (Reverse), getReverse)

-- writer-cps-transformers imports for re-export
import Control.Monad.Trans.Writer.CPS as Export
  ( execWriter, execWriterT, runWriter, runWriterT, Writer, WriterT
  , writer, writerT
  )

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
import Data.Functor.Plus as Export
  ( Plus
  , zero
  )
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

-- directory imports for re-export
import System.Directory as Export
  (createDirectoryIfMissing, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile, withCurrentDirectory, getCurrentDirectory)

-- lens imports for re-export
import Control.Lens as Export
  (Lens', set, over, view, views, (<>~), Iso, Iso', AnIso, AnIso', from, iso, withIso, Prism, Prism', APrism, APrism', preview, matching, review, withPrism, foldMapOf, anyOf, (<+=), (<+~))

-- monoidal-containers imports for re-export
import Data.Map.Monoidal as Export
  ( MonoidalMap ( MonoidalMap, getMonoidalMap )
  )

-- invariant imports for re-export
import Data.Functor.Invariant as Export
  ( Invariant (invmap), invmapFunctor, invmapContravariant
  )

-- profunctors imports for re-export
import Data.Profunctor as Export
  (Profunctor, dimap)

-- either imports for re-export
import Data.Either.Validation as Export
  (Validation, validationToEither, eitherToValidation)

-- template-haskell imports for re-export
import Language.Haskell.TH.Syntax as Export
  ( Lift
  )

-- temporary imports for re-export
import System.IO.Temp as Export
  ( withSystemTempDirectory
  )

-- time imports for re-export
import Data.Time.Calendar as Export
  ( Day ( ModifiedJulianDay )
  , addDays
  , toGregorian
  )
import Data.Time.Calendar.OrdinalDate as Export
  ( toOrdinalDate
  )
import Data.Time.Clock as Export
  ( DiffTime
  , NominalDiffTime
  , UTCTime
  , addUTCTime
  , diffTimeToPicoseconds
  , diffUTCTime
  , getCurrentTime
  , picosecondsToDiffTime
  )
import Data.Time.Clock.TAI as Export
  ( AbsoluteTime
  , LeapSecondMap
  , diffAbsoluteTime
  , taiEpoch
  , taiNominalDayStart
  , utcToTAITime
  )
import Data.Time.LocalTime as Export
  ( LocalTime ( LocalTime )
  , ZonedTime ( ZonedTime )
  , getZonedTime
  , localDay
  , localTimeOfDay
  , localTimeToUTC
  , midnight
  , timeOfDayToTime
  , timeZoneOffsetString
  , todHour
  , todMin
  , todSec
  , zonedTimeToLocalTime
  , zonedTimeToUTC
  , zonedTimeZone
  )

-- time-compat imports for re-export
-- TODO: import from time when we can, which is likely when GHC 8.6 support gets dropped
import Data.Time.Calendar.Compat as Export
  ( DayOfWeek ( Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday )
  , dayOfWeek
  )
import Data.Time.Clock.Compat as Export
  ( nominalDiffTimeToSeconds
  , secondsToNominalDiffTime
  )

-- Q4C12 packages for re-export
import Q4C12.FoldableUtils as Export
  ( intercalate0, intercalateMap0, biintercalateMap0
  , foldMapM, foldSequence, bifoldMapM
  , prependsMap, prepends, appendsMap, appends
  , unfoldr'
  )
import Q4C12.HList as Export
  (HSum (HSumHere, HSumThere), absurdHSum, eliminateHSum, _HSumHere, _HSumThere, eitherSum, HProd (HProdCons, HProdNil), headL, tailL, unitProd, singleProd, doubleProd, dropUnit)
import Q4C12.TwoFinger as Export
  (TwoFingerOddA, TwoFingerOddE, TwoFingerEvenA, TwoFingerEvenE)

--Imports for local use.
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed
  ( divMod'
  )
import Data.Semigroup (mtimesDefault)
import Data.String (String)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import GHC.Real (fromIntegral, RealFrac, truncate)

type SText = ST.Text
type LText = LT.Text
type TBuilder = LTB.Builder

type SByteString = SBS.ByteString
type LByteString = LBS.ByteString
type BSBuilder = BSB.Builder

--TODO: get rid of all the local definitions that aren't purely for compatibility.

mtimesSafe :: (Monoid a) => Natural -> a -> a
mtimesSafe = mtimesDefault . (fromIntegral :: Natural -> Integer)

-- TODO: these are both from errors; maybe think about incurring a dep on that?
hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = maybe empty pure

type FilePathComponent = String

validationToExcept :: (Applicative f) => Validation e a -> ExceptT e f a
validationToExcept = mapExceptT (pure . runIdentity) . except . validationToEither

exceptToValidation :: Except e a -> Validation e a
exceptToValidation = eitherToValidation . runExcept

failure :: e -> Validation e a
failure = eitherToValidation . Left

truncateInteger :: ( RealFrac a ) => a -> Integer
truncateInteger = truncate

divModInteger :: ( RealFrac a ) => a -> a -> ( Integer, a )
divModInteger = divMod'

fromIntegerClip :: ( Bounded a, Integral a ) => Integer -> a
fromIntegerClip = f Proxy
  where
  f :: ( Bounded a, Integral a ) => proxy a -> Integer -> a
  f proxy input =
    if inUpper && inLower
    then fromIntegral input
    else if inUpper
    then lo
    else hi
    where
    hi = maxBound `asProxyTypeOf` proxy
    lo = minBound `asProxyTypeOf` proxy
    inUpper = fromIntegral hi >= input
    inLower = fromIntegral lo <= input
