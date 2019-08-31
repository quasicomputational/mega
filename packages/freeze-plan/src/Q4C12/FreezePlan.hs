{-# LANGUAGE TypeFamilies #-}
module Q4C12.FreezePlan
  ( freezeConstraints
  , FreezeConstraintsError
  , formatError
  )
  where

import qualified Cabal.Plan as Plan
import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.Map as Map
import qualified Data.Map.Internal as Map.Internal
import qualified Data.Map.Merge.Lazy as Merge
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text.Lazy.Builder as LTB
import qualified Distribution.Compat.Graph as Graph
import qualified Distribution.Types.GenericPackageDescription as GPD
import qualified Distribution.Package as Package
import qualified Distribution.Version as Version
import qualified Q4C12.ProjectFile as PF

mapOfSetsToSet :: Map k ( Set a ) -> Set ( k, a )
mapOfSetsToSet = Map.keysSet . assocMaps . fmap ( Map.fromSet ( const () ) )

-- TODO: upstream?
assocMaps :: Map a ( Map b c ) -> Map ( a, b ) c
assocMaps = \case
  Map.Internal.Tip -> Map.Internal.Tip
  Map.Internal.Bin _ k v l r
    -> Map.Internal.link2 ( assocMaps l )
     $ Map.Internal.link2 ( Map.mapKeysMonotonic ( (,) k ) v )
     $ assocMaps r

isLocal :: Plan.Unit -> Bool
isLocal unit = case Plan.uType unit of
  Plan.UnitTypeBuiltin -> False
  Plan.UnitTypeGlobal -> False
  Plan.UnitTypeLocal -> True
  Plan.UnitTypeInplace -> True

packageName :: Plan.Unit -> Plan.PkgName
packageName ( Plan.uPId -> Plan.PkgId name _ver ) = name

packageVersion :: Plan.Unit -> Plan.Ver
packageVersion ( Plan.uPId -> Plan.PkgId _name ver ) = ver

newtype UnitNode = UnitNode { getUnitNode :: Plan.Unit }

-- Considers all of its dependencies, qualified and not, executable or library, to be its neighbours.
instance Graph.IsNode UnitNode where
  type Key UnitNode = Plan.UnitId

  nodeKey = Plan.uId . getUnitNode

  nodeNeighbors
      = getUnitNode
    >>> Plan.uComps
    >>> foldMap ( \ comp -> Plan.ciLibDeps comp <> Plan.ciExeDeps comp )
    >>> toList

-- TODO: more efficient algorithm
toGraph :: Plan.PlanJson -> Graph.Graph UnitNode
toGraph = foldr Graph.insert Graph.empty . fmap UnitNode . Plan.pjUnits

data Qualified
  = QualifiedSetup Plan.PkgName
  | QualifiedExe Plan.PkgName
  deriving stock ( Eq, Ord, Show )

qualifiedDepends :: Plan.Unit -> Set ( Qualified, Plan.UnitId )
qualifiedDepends unit = mapOfSetsToSet $ Map.fromList
  [ (,) ( QualifiedSetup pn ) $ foldMap Plan.ciLibDeps $ Map.lookup Plan.CompNameSetup $ Plan.uComps unit
  , (,) ( QualifiedExe pn ) $ foldMap Plan.ciExeDeps $ Plan.uComps unit
  ]
  where
  pn = packageName unit

unqualifiedDepends :: Plan.Unit -> Set Plan.UnitId
unqualifiedDepends = foldMap Plan.ciLibDeps . Map.delete Plan.CompNameSetup . Plan.uComps

data PlanConstraints = PlanConstraints
  { _qualified :: MonoidalMap Plan.PkgName ( Map Qualified ( Plan.Ver, Map Plan.FlagName Bool ) )
  , _unqualified :: Map Plan.PkgName ( Plan.Ver, Map Plan.FlagName Bool )
  }

instance Semigroup PlanConstraints where
  -- In a build plan, two identically qualified packages with the same
  -- name must be identical, so freely pick the left-hand side's
  -- version and flags.
  PlanConstraints qa ua <> PlanConstraints qb ub = PlanConstraints
    ( qa <> qb )
    ( ua <> ub )

instance Monoid PlanConstraints where
  mempty = PlanConstraints mempty mempty

data FreezeConstraint
  = FreezeConstraintVersion Plan.Ver
  | FreezeConstraintFlag Plan.FlagName Bool
  deriving stock ( Eq, Ord )

isVersionConstraint :: FreezeConstraint -> Bool
isVersionConstraint = \case
  FreezeConstraintVersion{} -> True
  FreezeConstraintFlag{} -> False

fromPlanConstraints
  :: PlanConstraints
  -> MonoidalMap
       Plan.PkgName
       ( Map ( Maybe Qualified ) ( Plan.Ver, Map Plan.FlagName Bool ) )
fromPlanConstraints ( PlanConstraints qualified unqualified ) =
  MonoidalMap $ Merge.merge
    ( Map.singleton Nothing <$> Merge.preserveMissing )
    Merge.preserveMissing
    ( Merge.zipWithMatched $ \ _ -> Map.insert Nothing )
    unqualified
    ( Map.mapKeysMonotonic Just <$> getMonoidalMap qualified )

data Same a = Same a | Different

instance ( Eq a ) => Semigroup ( Same a ) where
  Different <> _ = Different
  _ <> Different = Different
  Same a <> Same b = if a == b then Same a else Different

toFreezeConstraints
  :: Plan.PkgName
  -> Map ( Maybe Qualified ) ( Plan.Ver, Map Plan.FlagName Bool )
  -> Either
       FreezeConstraintsError
       ( Set ( PF.Qualification, FreezeConstraint ) )
toFreezeConstraints dep constrs =
  case runWriter $ getCompose $ itraverse go constrs of
    ( Nothing, Nothing ) -> Right mempty
    ( Nothing, Just Different ) -> Left $ UnfreezableExeDependency dep
    ( Nothing, Just ( Same ( v, fs ) ) ) -> Right $ constraints PF.qualifiedAll v fs
    ( Just res, _ ) -> Right $ fold res
  where

  go
    :: Maybe Qualified
    -> ( Plan.Ver, Map Plan.FlagName Bool )
    -> Compose
         ( Writer ( Maybe ( Same ( Plan.Ver, Map Plan.FlagName Bool ) ) ) )
         Maybe
         ( Set ( PF.Qualification, FreezeConstraint ) )
  go qual ( v, fs ) = Compose $ do
    Writer.tell ( Just $ Same ( v, fs ) )
    pure $ case qual of
      Nothing -> Just $ constraints PF.unqualifiedOnly v fs
      Just ( QualifiedSetup ( Plan.PkgName ( ST.unpack -> pkg ) ) ) -> Just $ constraints ( PF.qualifiedSetup ( Package.mkPackageName pkg ) ) v fs
      Just QualifiedExe{} -> Nothing

  constraints :: ( Ord q ) => q -> Plan.Ver -> Map Plan.FlagName Bool -> Set ( q, FreezeConstraint )
  constraints qual v fs = fold
    [ Set.singleton ( qual, FreezeConstraintVersion v )
    , flip ifoldMap fs $ \ flag enabled ->
        Set.singleton ( qual, FreezeConstraintFlag flag enabled )
    ]

toPFConstraints
  :: Plan.PkgName
  -> Set ( PF.Qualification, FreezeConstraint )
  -> Seq PF.Constraint
toPFConstraints ( Plan.PkgName ( Package.mkPackageName . ST.unpack -> pn ) ) = foldMap $ \ ( qual, constr ) -> Seq.singleton $ case constr of
  FreezeConstraintVersion ( Plan.Ver v ) ->
    PF.constraintVersion pn qual ( Version.thisVersion $ Version.mkVersion v )
  FreezeConstraintFlag ( Plan.FlagName ( ST.unpack -> flag ) ) enabled ->
    PF.constraintFlag pn qual enabled ( GPD.mkFlagName flag )

data FreezeConstraintsError
  = UnfreezableExeDependency Plan.PkgName
  | DanglingUnitId Plan.UnitId
  | PackageDependencyCycle
  deriving stock ( Eq, Ord )

formatError :: FreezeConstraintsError -> TBuilder
formatError = \case
  UnfreezableExeDependency ( Plan.PkgName name ) -> fold
    [ "The plan has a dependency on an executable in package ", LTB.fromText name, ". This is not yet supported because Cabal doesn't have a syntax to write that constraint. Further, there are other dependencies on this package with conflicting versions, so an any-component constraint is impossible.\n"
    ]
  DanglingUnitId ( Plan.UnitId uid ) -> fold
    [ "Internal error: dangling unit id ", LTB.fromText uid, ".\n"
    ]
  PackageDependencyCycle ->
    "There is a package dependency cycle. Either that's a bug or cabal-install has learnt new tricks.\n"

freezeConstraints
  :: Plan.PlanJson
  -> Validation
       ( Set FreezeConstraintsError )
       ( Seq PF.Constraint )
freezeConstraints plan
  = fmap ( ifoldMap toPFConstraints )
  $ fmap deleteLocalPackagesVersionConstraints
  $ eitherToValidation
  $ (=<<) ( validationToEither . itraverse ( \ k -> first Set.singleton . eitherToValidation . toFreezeConstraints k ) )
  $ fmap fromPlanConstraints
  $ fmap fold
  $ fmap ( flip Map.restrictKeys localUnitIds )
  $ transitiveClosures
  where
  --TODO: test cases:
  --  - transitively sucks in qualified constraints via unqualified
  --  - transitively sucks in qualified constraints via qualified constraints
  --  - unqualified transitive deps get qualified
  --  - with build-tool-depends, internal and external
  --  - with multilibs, internal and external
  --  - where the dependency has backpack holes
  --  - package graph cycle: lib:A -> lib:B -> test:A
  --  - build-tool-depends cycle involving two packages: exe:A:a1 -> exe:B:b1, and exe:B:b2 -> exe:A:a2
  --  - where a build-tool-dependency has backpack holes
  --  - where a setup dependency has backpack holes
  --  - where a local package has flags
  --  - where a local package is used as a setup dependency of another local package
  --  - where a setup dependency has flags

  -- TODO: this takes shortcuts because it knows that the solver works at the package level, not components, and hence won't ever create a loop in the package graph. When that's no longer true, we'll have to do a two-pass process. First step computes the transitive closure of all the unqualified dependencies (for the whole graph); do bear in mind it has to be over SCCs. Second step then looks at the SCCs of the graph of all dependencies (qualified and not): both intra-SCC qualified dependencies and (transitive!) extra-SCC qualified dependencies are gathered up and then applied to all members of the SCC.
  transitiveClosures
    :: Either
         ( Set FreezeConstraintsError )
         ( Map Plan.UnitId PlanConstraints )
  transitiveClosures = foldM processSCC mempty sccs
    where

    sccs = Graph.stronglyConnComp $ toGraph plan

    processSCC deps scc = case scc of
      Graph.AcyclicSCC ( UnitNode pkg ) -> do
        constraints <- validationToEither $ transitiveClosure deps pkg
        pure $ Map.insert ( Plan.uId pkg ) constraints deps
      Graph.CyclicSCC _ ->
        Left ( Set.singleton PackageDependencyCycle )

  transitiveClosure
    :: Map Plan.UnitId PlanConstraints
    -> Plan.Unit
    -> Validation ( Set FreezeConstraintsError ) PlanConstraints
  transitiveClosure deps pkg = foldSequence
    [ pure base
    , foldMapM lookupUnqualified ( unqualifiedDepends pkg )
    , foldMapM lookupQualified ( qualifiedDepends pkg )
    ]
    where

    lookupUnqualified unitId = case Map.lookup unitId deps of
      Nothing -> failure $ Set.singleton $ DanglingUnitId unitId
      Just constr -> pure $ constr

    lookupQualified (qualification, unitId) = case Map.lookup unitId deps of
      Nothing -> failure $ Set.singleton $ DanglingUnitId unitId
      Just ( PlanConstraints qualified unqualified ) -> pure $ PlanConstraints
        ( qualified <> ( Map.singleton qualification <$> MonoidalMap unqualified ) )
        mempty

    base = PlanConstraints
      mempty
      ( Map.singleton
          ( packageName pkg )
          ( packageVersion pkg, Plan.uFlags pkg )
      )

  deleteLocalPackagesVersionConstraints
    :: MonoidalMap Plan.PkgName ( Set ( a, FreezeConstraint ) )
    -> MonoidalMap Plan.PkgName ( Set ( a, FreezeConstraint ) )
  deleteLocalPackagesVersionConstraints constraintMap =
    MonoidalMap $ Merge.merge
      Merge.dropMissing
      Merge.preserveMissing
      ( Merge.zipWithMatched $ \ _pkgName () -> Set.filter ( not . isVersionConstraint . snd ) )
      localPackages
      ( getMonoidalMap constraintMap )

  localPackages :: Map Plan.PkgName ()
  localPackages =
    foldMap
      ( \ unit -> if isLocal unit
        then Map.singleton ( packageName unit ) ()
        else mempty
      )
      ( Plan.pjUnits plan )

  localUnitIds :: Set Plan.UnitId
  localUnitIds = Map.keysSet $ filter isLocal ( Plan.pjUnits plan )
