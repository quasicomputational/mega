module Q4C12.Refrigerate
  ( refrigerate
  )
  where

import Control.Lens
  ( _1
  )
import qualified Data.ByteString as BS
import qualified Data.DList.NonEmpty as NEDL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST
import Distribution.PackageDescription
  ( Benchmark
  , Executable
  , GenericPackageDescription ( GenericPackageDescription )
  , Library
  , PackageDescription ( benchmarks, executables, foreignLibs, library, setupBuildInfo, subLibraries, testSuites )
  , SetupBuildInfo
  , TestSuite
  )
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription
  , runParseResult
  )
import Distribution.Parsec.Common
  ( showPError
  )
import Distribution.Types.Dependency
  ( Dependency ( Dependency )
  )
import Distribution.Types.ForeignLib
  ( ForeignLib
  )
import qualified Distribution.Types.Lens as Lenses
import Distribution.Types.PackageName
  ( PackageName
  , unPackageName
  )
import Distribution.Types.UnqualComponentName
  ( packageNameToUnqualComponentName
  )
import Distribution.Types.VersionInterval
  ( Bound ( InclusiveBound )
  , LowerBound ( LowerBound )
  , UpperBound ( UpperBound )
  , asVersionIntervals
  , fromVersionIntervals
  , toVersionIntervals
  )
import Distribution.Types.VersionRange
  ( VersionRange
  , intersectVersionRanges
  , majorBoundVersion
  , unionVersionRanges
  )
import Q4C12.ProjectFile
  ( Config
  , Constraint
  , constraints
  , constraintAppliesToSetup
  , constraintAppliesToUnqualified
  , constraintPackageName
  , constraintToVersionRange
  )

--TODO: accept a config detailing which packages are semver, not PVP; we can default to PVP because PVP-bounding a semver package is safe (but conservative). So semver fixes one component and PVP fixes two; do I want to go one, two, N??

-- Strictly speaking, we don't need to canonicalise. However, for output nicety (and test output stability), we do.
canonicalVersionRange :: VersionRange -> VersionRange
canonicalVersionRange = fromVersionIntervals . toVersionIntervals

data RefrigerationError
  = UnfrozenDependency QualificationType PackageName
  | NotFullyFrozen PackageName

showRefrigerationError :: RefrigerationError -> SText
showRefrigerationError = \case
  UnfrozenDependency Unqualified pn ->
    "Unfrozen unqualified dependency: " <> ST.pack ( unPackageName pn ) <> "\n"
  UnfrozenDependency Setup pn ->
    "Unfrozen setup dependency: " <> ST.pack ( unPackageName pn ) <> "\n"
  NotFullyFrozen pn ->
    "Loose bounds in the freeze file: " <> ST.pack ( unPackageName pn ) <> "\n"

data QualificationType = Unqualified | Setup
  deriving (Eq, Ord)

class DependencyWithQualification a where
  traverseDependencies
    :: (Applicative f)
    => (QualificationType -> Dependency -> f Dependency)
    -> a
    -> f a

instance DependencyWithQualification GenericPackageDescription where
  traverseDependencies f (GenericPackageDescription pd flags clib csublibs cflibs cexes ctests cbenchs) =
    GenericPackageDescription
      <$> traverseDependencies f pd
      <*> pure flags
      <*> traverse (traverse $ traverseDependencies f) clib
      <*> traverse (traverse $ traverse $ traverseDependencies f) csublibs
      <*> traverse (traverse $ traverse $ traverseDependencies f) cflibs
      <*> traverse (traverse $ traverse $ traverseDependencies f) cexes
      <*> traverse (traverse $ traverse $ traverseDependencies f) ctests
      <*> traverse (traverse $ traverse $ traverseDependencies f) cbenchs

instance DependencyWithQualification PackageDescription where
  traverseDependencies f pd = recombine
    <$> traverse (traverseDependencies f) (setupBuildInfo pd)
    <*> traverse (traverseDependencies f) (library pd)
    <*> traverse (traverseDependencies f) (subLibraries pd)
    <*> traverse (traverseDependencies f) (executables pd)
    <*> traverse (traverseDependencies f) (foreignLibs pd)
    <*> traverse (traverseDependencies f) (testSuites pd)
    <*> traverse (traverseDependencies f) (benchmarks pd)
    where
    recombine setup lib sublibs exes flibs tests benchs = pd
      { setupBuildInfo = setup
      , library = lib
      , subLibraries = sublibs
      , executables = exes
      , foreignLibs = flibs
      , testSuites = tests
      , benchmarks = benchs
      }

instance DependencyWithQualification SetupBuildInfo where
  traverseDependencies f =
    Lenses.setupDepends . traverse $ (f Setup)

traverseDependenciesBuildInfo
  :: ( Lenses.HasBuildInfo a, Applicative f )
  => ( QualificationType -> Dependency -> f Dependency )
  -> a
  -> f a
traverseDependenciesBuildInfo f =
  Lenses.buildInfo . Lenses.targetBuildDepends . traverse $ f Unqualified

instance DependencyWithQualification Library where
  traverseDependencies = traverseDependenciesBuildInfo

instance DependencyWithQualification ForeignLib where
  traverseDependencies = traverseDependenciesBuildInfo

instance DependencyWithQualification Executable where
  traverseDependencies = traverseDependenciesBuildInfo

instance DependencyWithQualification TestSuite where
  traverseDependencies = traverseDependenciesBuildInfo

instance DependencyWithQualification Benchmark where
  traverseDependencies = traverseDependenciesBuildInfo

type FrozenDependencies =
  Map (PackageName, QualificationType) (Validation (NonEmptyDList RefrigerationError) VersionRange)

applyFreezes
  :: FrozenDependencies
  -> GenericPackageDescription
  -> Validation (NonEmptyDList RefrigerationError) GenericPackageDescription
applyFreezes frozen gpd =
  traverseDependencies addBounds gpd

  where

  isInternal pn
     = anyOf
         ( Lenses.packageDescription . Lenses.package . Lenses.pkgName )
         (== pn)
         gpd
    || anyOf
         ( Lenses.condSubLibraries . traverse . _1 )
         (== packageNameToUnqualComponentName pn)
         gpd

  -- TODO: not sure this is doing the right thing for internal libraries
  addBounds qual dep@(Dependency pn range) =
    case Map.lookup ( pn, qual ) frozen of
      Nothing
        | isInternal pn ->
          pure dep
        | otherwise ->
          failure $ NEDL.singleton $ UnfrozenDependency qual pn
      Just range' ->
          Dependency pn . canonicalVersionRange . intersectVersionRanges range <$> range'

-- Make sure that the dependencies are actually frozen. The validation goes inside the map because we might not ever need those constraints; if there are imprecise or broken ones for non-immediate deps, we can just ignore those.
checkFrozenDependencies
  :: Map (PackageName, QualificationType) VersionRange
  -> FrozenDependencies
checkFrozenDependencies =
  Map.mapWithKey $ \ (pn, _) range -> case asVersionIntervals range of
    [(LowerBound lb InclusiveBound, UpperBound ub InclusiveBound)]
      | lb == ub ->
        pure $ majorBoundVersion lb
    _ ->
        failure $ NEDL.singleton $ NotFullyFrozen pn

--TODO: build-tools, build-tool-depends
--TODO tests for not-fully-frozen ranges and other weird cases
gatherFreezes
  :: PackageName
  -- ^ Name of the package description we're refrigerating.
  -> [Config]
  -> FrozenDependencies
gatherFreezes pkgName
    = fmap ( view constraints
         >>> fmap constraintToQualifiedMap
         >>> toList
         >>> Map.unionsWith intersectVersionRanges
         >>> checkFrozenDependencies
           )
  >>> Map.unionsWith ( liftA2 unionVersionRanges )
  where

  filterIrrelevant constr = List.filter $ \ (_, qual) -> case qual of
    Unqualified -> constraintAppliesToUnqualified constr
    Setup -> constraintAppliesToSetup pkgName constr

  constraintToQualifiedMap
    :: Constraint
    -> Map (PackageName, QualificationType) VersionRange
  constraintToQualifiedMap constr =
    Map.fromSet ( const $ constraintToVersionRange constr ) $
      Set.fromList $ filterIrrelevant constr
        [ ( constraintPackageName constr, Unqualified )
        , ( constraintPackageName constr, Setup )
        ]

refrigerate
  :: [Config]
  -> BS.ByteString
  -> Either SText GenericPackageDescription
refrigerate projectConfigs input = do
  let parseRes = parseGenericPackageDescription input
  case runParseResult parseRes of
    (_warns, Left (_versionMay, errs)) ->
      Left $ ST.pack $ foldMap (showPError "(input)") errs
    (_warns, Right gpd) -> do
      let
        pn = view ( Lenses.packageDescription . Lenses.package . Lenses.pkgName ) gpd
        frozen = gatherFreezes pn projectConfigs
      --TODO: should be sorting the errors, for test stability
      first (foldMap showRefrigerationError) $ validationToEither $ applyFreezes frozen gpd
