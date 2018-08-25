{-# LANGUAGE FlexibleInstances #-}

module Q4C12.Defrost
  ( env
  , Env
  , defrost
  )
  where

import Control.Lens
  ( _1
  , _2
  )
import qualified Data.DList.NonEmpty as NEDL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST
import Distribution.Compiler
  ( CompilerFlavor
  )
import Distribution.PackageDescription
  ( Benchmark
  , Condition ( CAnd, CNot, COr, Lit, Var )
  , CondTree ( CondNode )
  , ConfVar ( Arch, Flag, Impl, OS )
  , Executable
  , FlagAssignment
  , GenericPackageDescription ( GenericPackageDescription )
  , Library
  , PackageDescription ( benchmarks, executables, foreignLibs, library, setupBuildInfo, subLibraries, testSuites )
  , SetupBuildInfo
  , TestSuite
  , lookupFlagAssignment
  )
import Distribution.System
  ( Arch
  , OS
  )
import Distribution.Types.CondTree
  ( CondBranch ( CondBranch )
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
  , noVersion
  , unionVersionRanges
  )
import Distribution.Version
  ( Version
  , withinRange
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

data SystemEnv = SystemEnv
  { _systemEnvOS :: OS
  , _systemEnvArch :: Arch
  , _systemEnvFlags :: FlagAssignment
  , _systemEnvCompilerFlavor :: CompilerFlavor
  , _systemEnvCompilerVersion :: Version
  }
  deriving ( Eq, Ord )

data Env = Env SystemEnv Config

env :: OS -> Arch -> FlagAssignment -> CompilerFlavor -> Version -> Config -> Env
env os arch flags compiler compilerVersion =
  Env ( SystemEnv os arch flags compiler compilerVersion )

-- Strictly speaking, we don't need to canonicalise. However, for output nicety (and test output stability), we do.
canonicalVersionRange :: VersionRange -> VersionRange
canonicalVersionRange = fromVersionIntervals . toVersionIntervals

data DefrostingError
  = UnfrozenDependency QualificationType PackageName
  | NotFullyFrozen PackageName SystemEnv

showDefrostingError :: DefrostingError -> SText
showDefrostingError = \case
  UnfrozenDependency Build pn ->
    "Unfrozen build dependency: " <> ST.pack ( unPackageName pn ) <> "\n"
  UnfrozenDependency Setup pn ->
    "Unfrozen setup dependency: " <> ST.pack ( unPackageName pn ) <> "\n"
  -- TODO: use systemEnv in the output!
  NotFullyFrozen pn _systemEnv ->
    "Loose bounds in the freeze file: " <> ST.pack ( unPackageName pn ) <> "\n"

data QualificationType = Build | Setup
  deriving (Eq, Ord)

class DependencyWithQualification a where
  traverseDependencies
    :: ( Applicative f )
    => ( QualificationType -> Condition ConfVar -> Dependency -> f Dependency )
    -> a
    -> f a

instance DependencyWithQualification GenericPackageDescription where
  traverseDependencies f (GenericPackageDescription pd flags clib csublibs cflibs cexes ctests cbenchs) =
    GenericPackageDescription
      <$> traverseDependencies f pd
      <*> pure flags
      <*> traverse (traverseDependencies f) clib
      <*> traverse (traverse $ traverseDependencies f) csublibs
      <*> traverse (traverse $ traverseDependencies f) cflibs
      <*> traverse (traverse $ traverseDependencies f) cexes
      <*> traverse (traverse $ traverseDependencies f) ctests
      <*> traverse (traverse $ traverseDependencies f) cbenchs

-- Note: this results in the 'condTreeConstraints' being wrong. We fix that up as a post-processing step, since it's strictly redundant information.
instance ( DependencyWithQualification a )
      => DependencyWithQualification ( CondTree ConfVar cs a ) where
  traverseDependencies f ( CondNode a cs branches ) = CondNode
    <$> traverseDependencies f a
    <*> pure cs
    <*> traverse ( traverseDependencies f ) branches

instance ( DependencyWithQualification a )
      => DependencyWithQualification ( CondBranch ConfVar cs a ) where
  traverseDependencies f ( CondBranch cond true falseMay ) = CondBranch cond
    <$> traverseDependencies ( \ qual -> f qual . CAnd cond ) true
    <*> traverse ( traverseDependencies $ \ qual -> f qual . CAnd ( CNot cond ) ) falseMay

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
    Lenses.setupDepends . traverse $ f Setup (Lit True)

traverseDependenciesBuildInfo
  :: ( Lenses.HasBuildInfo a, Applicative f )
  => ( QualificationType -> Condition ConfVar -> Dependency -> f Dependency )
  -> a
  -> f a
traverseDependenciesBuildInfo f
  = Lenses.buildInfo
  . Lenses.targetBuildDepends
  . traverse
  $ f Build (Lit True)

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
  Map ( PackageName, QualificationType )
    ( Map SystemEnv
        ( Validation ( NonEmptyDList DefrostingError ) VersionRange )
    )

newtype UnionRanges =
  UnionRanges { getUnionRanges :: VersionRange }

instance Semigroup UnionRanges where
  UnionRanges a <> UnionRanges b =
    UnionRanges $ unionVersionRanges a b

instance Monoid UnionRanges where
  mempty = UnionRanges noVersion
  mappend = (<>)

applyFreezes
  :: FrozenDependencies
  -> GenericPackageDescription
  -> Validation (NonEmptyDList DefrostingError) GenericPackageDescription
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
  addBounds
    :: QualificationType
    -> Condition ConfVar
    -> Dependency
    -> Validation ( NonEmptyDList DefrostingError ) Dependency
  addBounds qual cond dep@(Dependency pn range) = do
    let
      relevant = frozen
        & Map.lookup ( pn, qual )
        & fold
        & Map.filterWithKey ( \ systemEnv _ -> matchesCondition systemEnv cond )
    if Map.null relevant
      then if isInternal pn
        then pure dep
        else failure $ NEDL.singleton $ UnfrozenDependency qual pn
      else Dependency pn . canonicalVersionRange . intersectVersionRanges range . getUnionRanges <$> foldMapM ( fmap UnionRanges ) relevant

matchesCondition :: SystemEnv -> Condition ConfVar -> Bool
matchesCondition ( SystemEnv os arch flags compiler compilerVersion ) =
  evalCondition . fmap evalConfVar
  where
    evalCondition ( Var b ) = b
    evalCondition ( Lit b ) = b
    evalCondition ( CNot c ) = not ( evalCondition c )
    evalCondition ( CAnd a b ) = evalCondition a && evalCondition b
    evalCondition ( COr a b ) = evalCondition a || evalCondition b

    evalConfVar ( OS os' ) =
      os == os'
    evalConfVar ( Arch arch' ) =
      arch == arch'
    -- Since this package can't've been built (since Cabal would barf), we can do anything we like. In this instance, assume that a missing flag isn't set.
    evalConfVar ( Flag flag ) =
      fromMaybe False $ lookupFlagAssignment flag flags
    evalConfVar ( Impl compiler' range ) =
      compiler == compiler' && withinRange compilerVersion range

-- Make sure that the dependencies are actually frozen. The validation goes inside the map because we might not ever need those constraints; if there are imprecise or broken ones for non-immediate deps, we can just ignore those.
checkFrozenDependencies
  :: Map ( PackageName, QualificationType )
       ( Map SystemEnv VersionRange )
  -> FrozenDependencies
checkFrozenDependencies =
  Map.mapWithKey $ \ (pn, _) -> Map.mapWithKey $ \ systemEnv range ->
    case asVersionIntervals range of
      [(LowerBound lb InclusiveBound, UpperBound ub InclusiveBound)]
        | lb == ub ->
          pure $ majorBoundVersion lb
      _ ->
          failure $ NEDL.singleton $ NotFullyFrozen pn systemEnv

--TODO: build-tools, build-tool-depends
--TODO tests for not-fully-frozen ranges and other weird cases
gatherFreezes
  :: PackageName
  -- ^ Name of the package description we're defrosting.
  -> [ Env ]
  -> FrozenDependencies
gatherFreezes pkgName
    = fmap ( \ ( Env buildEnv config )
             -> view constraints config
              & fmap constraintToQualifiedMap
              & toList
              & Map.unionsWith intersectVersionRanges
              & fmap ( Map.singleton buildEnv )
              & checkFrozenDependencies
           )
  >>> Map.unionsWith ( Map.unionWith $ liftA2 unionVersionRanges )
  where

  filterIrrelevant constr = List.filter $ \ (_, qual) -> case qual of
    Build -> constraintAppliesToUnqualified constr
    Setup -> constraintAppliesToSetup pkgName constr

  constraintToQualifiedMap
    :: Constraint
    -> Map (PackageName, QualificationType) VersionRange
  constraintToQualifiedMap constr =
    Map.fromSet ( const $ constraintToVersionRange constr ) $
      Set.fromList $ filterIrrelevant constr
        [ ( constraintPackageName constr, Build )
        , ( constraintPackageName constr, Setup )
        ]

fixGPDConstraints
  :: GenericPackageDescription
  -> GenericPackageDescription
fixGPDConstraints
  = over ( Lenses.condBenchmarks . traverse . _2 ) fixCondTreeConstraints
  . over ( Lenses.condExecutables . traverse . _2 ) fixCondTreeConstraints
  . over ( Lenses.condForeignLibs . traverse . _2 ) fixCondTreeConstraints
  . over ( Lenses.condLibrary . traverse ) fixCondTreeConstraints
  . over ( Lenses.condSubLibraries . traverse . _2 ) fixCondTreeConstraints
  . over ( Lenses.condTestSuites . traverse . _2 ) fixCondTreeConstraints

fixCondTreeConstraints
  :: ( Lenses.HasBuildInfo a )
  => CondTree v cs a
  -> CondTree v [Dependency] a
fixCondTreeConstraints ( CondNode a _ branches ) =
  CondNode a deps ( fixCondBranchConstraints <$> branches )
  where
  deps = view ( Lenses.buildInfo . Lenses.targetBuildDepends ) a

fixCondBranchConstraints
  :: ( Lenses.HasBuildInfo a )
  => CondBranch v cs a
  -> CondBranch v [Dependency] a
fixCondBranchConstraints ( CondBranch cond true falseMay ) =
  CondBranch cond
    ( fixCondTreeConstraints true )
    ( fixCondTreeConstraints <$> falseMay )

defrost
  :: [ Env ]
  -> GenericPackageDescription
  -> Either SText GenericPackageDescription
defrost envs gpd = do
  let
    pn = view ( Lenses.packageDescription . Lenses.package . Lenses.pkgName ) gpd
    frozen = gatherFreezes pn envs
  --TODO: should be sorting the errors, for test stability
  bimap ( foldMap showDefrostingError ) fixGPDConstraints $ validationToEither $ applyFreezes frozen gpd
