-- #######################################################################
-- ############################## ATTENTION ##############################
-- #######################################################################
-- If you are editing this file, make sure to run 'cabal v2-run meta generate' afterwards.
-- #######################################################################
-- ############################## ATTENTION ##############################
-- #######################################################################
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Q4C12.Meta
  ( main
  )
  where

import qualified Cabal.Plan as Plan
import Control.Exception
  ( tryJust )
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Accum as Accum
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MonoidalMap
import Data.Map.Merge.Lazy ( mapMissing, mergeA, traverseMissing, zipWithMatched )
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Encoding as LTEnc
import qualified Data.Text.Lazy.IO as LTIO
import qualified Distribution.Compiler as Compiler
import Distribution.Package
  ( Dependency
  , depPkgName
  , packageName
  )
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription
  , runParseResult
  )
import Distribution.PackageDescription.PrettyPrint
  ( writeGenericPackageDescription
  )
import Distribution.Parsec
  ( showPError
  )
import Distribution.Simple.Utils
  ( tryFindPackageDesc
  )
import qualified Distribution.System
import Distribution.Types.BuildInfo.Lens
  ( HasBuildInfo
  , targetBuildDepends
  )
import Distribution.Types.CondTree
  ( CondTree
  , simplifyCondTree
  )
import Distribution.Types.GenericPackageDescription
  ( ConfVar ( OS, Arch, Flag, Impl )
  , GenericPackageDescription ( GenericPackageDescription )
  )
import Distribution.Types.GenericPackageDescription.Lens
  ( packageDescription
  )
import qualified Distribution.Types.PackageDescription as PD
import Distribution.Types.PackageName
  ( PackageName
  , unPackageName
  )
import Distribution.Types.SetupBuildInfo
  ( setupDepends
  )
import Distribution.Types.SourceRepo
  ( SourceRepo ( repoKind, repoTag )
  , RepoKind ( RepoHead, RepoThis )
  )
import Distribution.Types.VersionRange
  ( orLaterVersion
  , withinRange
  )
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version
  ( Version
  , mkVersion
  )
import qualified Options.Applicative as OA
import qualified Q4C12.Defrost as Defrost
import qualified Q4C12.FreezePlan as Freeze
import qualified Q4C12.ProjectFile as PF
import Q4C12.XML ( parseXML', displayWarnings, displayError, uname )
import Q4C12.XMLDesc
  ( Desc
  , El
  , EvenFlow
  , consolidate
  , elementMixed
  , flowWSEDropComments
  , flowEvenPreWSDropComments
  , oddTxNoPos
  , rcons
  , rfmap
  , rmany
  , rnil
  )
import qualified Q4C12.XMLDesc.Parse as XMLDesc.Parse
import System.Process
  ( callProcess
  )
import qualified System.Process as Proc

-- TODO: wouldn't it be cool to integrate this with SUPPORT.markdown, and have a machine-checked policy for older GHC compatibility?

-- There's a certain amount of duplicated work going on when two builds share a GHC version, but downloading from the PPA is SO SLOW that it's worth it; after the first slow runs it gets cached just fine. If the per-container overhead was reduced, then we could move back to aiming for no redundant builds of dependencies, but it's just too much as it is.

data Regularity
  = Regular
    { _regularGHCVersion :: Version
    }
  | PreRelease
    { _preReleaseCabal :: SText
    , _preReleaseGHCVersion :: Version
    }

data GHCVersion
  = GHC8_4
  | GHC8_6
  | GHC8_8
  | GHCHEAD
  deriving stock ( Eq, Ord )

-- This is the version that the package name is built from, hence lowercase 'head'.
ghcAptVersion :: GHCVersion -> SText
ghcAptVersion = \case
  GHC8_4 -> "8.4.4"
  GHC8_6 -> "8.6.5"
  GHC8_8 -> "8.8.1"
  GHCHEAD -> "head"

ghcCompilerVersion :: GHCVersion -> Version
ghcCompilerVersion ghc = case ghcRegularity ghc of
  Regular ver -> ver
  PreRelease _ ver -> ver

allCondTreesDependencies
  :: GenericPackageDescription
  -> DList ( CondTree ConfVar [ Dependency ] ( DList Dependency ) )
allCondTreesDependencies ( GenericPackageDescription _ _ mlib x1 x2 x3 x4 x5 ) = fold
  [ dlistOf ( traverse . deps ) mlib
  , dlistOf ( traverse . traverse . deps ) x1
  , dlistOf ( traverse . traverse . deps ) x2
  , dlistOf ( traverse . traverse . deps ) x3
  , dlistOf ( traverse . traverse . deps ) x4
  , dlistOf ( traverse . traverse . deps ) x5
  ]
  where
  -- Note: this is something even weaker than a 'Getter', as it
  -- doesn't need a 'Functor g' constraint. See
  -- https://gitlab.haskell.org/ghc/ghc/issues/12142 for why it's been
  -- expanded out.
  deps
    :: ( HasBuildInfo a, Functor f, Contravariant g )
    => ( f ( DList Dependency ) -> g ( f ( DList Dependency ) ) )
    -> f a
    -> g ( f a )
  deps = Lens.to $ fmap $ dlistOf ( targetBuildDepends . traverse )

allDependencies :: GHCVersion -> GenericPackageDescription -> Set PackageName
allDependencies ghcVer gpd = fold
  [ setOf ( packageDescription . Lens.to PD.setupBuildInfo . traverse . Lens.to setupDepends . traverse . Lens.to depPkgName ) gpd
  , setOf ( Lens.to allCondTreesDependencies . Lens.to ( fmap eval ) . Lens.to ( foldMap toList ) . traverse . Lens.to depPkgName ) gpd
  ]
  where
  --TODO: hard-coded knowledge of the Travis environment
  --TODO: punting on flags
  eval :: ( Monoid a, Monoid ds ) => CondTree ConfVar ds a -> a
  eval
    = snd
    . simplifyCondTree
      ( \case
          OS os ->
            Right ( os == Distribution.System.Linux )
          Arch arch ->
            Right ( arch == Distribution.System.X86_64 )
          Flag flag ->
            Left ( Flag flag )
          Impl compiler range ->
            Right ( compiler == Compiler.GHC && withinRange ( ghcCompilerVersion ghcVer ) range )
      )

ghcRegularity :: GHCVersion -> Regularity
ghcRegularity = \case
  GHC8_4 -> Regular (mkVersion [8, 4])
  GHC8_6 -> Regular (mkVersion [8, 6])
  GHC8_8 -> PreRelease "3.0" (mkVersion [8, 8])
  GHCHEAD -> PreRelease "head" (mkVersion [8, 10])

isRegular :: GHCVersion -> Bool
isRegular ghc = case ghcRegularity ghc of
  Regular{} -> True
  PreRelease{} -> False

data RunMetaChecks = MetaNo | MetaYes

data WError = WErrorNo | WErrorYes

data Package = Package
  { packageDirectory :: FilePath
  , packageGPD :: GenericPackageDescription
  }

data Build = Build
  { buildGHCVersion :: GHCVersion
  , buildPackages :: [Package]
  -- TODO: the values are really non-empty sets, but we can't really express that (yet).
  , buildExcludedPackages :: MonoidalMap PackageName ( Set PackageName )
  , buildRunMeta :: RunMetaChecks
  , buildWError :: WError
  }

env :: SText -> SText
env buildName = fold
  [ "PROJECT="
  , buildName
  ]

buildAllowFailures :: SText -> Build -> [Aeson.Encoding]
buildAllowFailures buildName build = do
  guard $ case ghcRegularity ( buildGHCVersion build ) of
    Regular{} -> False
    PreRelease{} -> True
  pure $ Aeson.Encoding.pairs $
    Aeson.Encoding.pair "env" $ Aeson.Encoding.text $ env buildName

travisConfiguration :: [(SText, Build)] -> Aeson.Encoding.Encoding
travisConfiguration buildMap = Aeson.Encoding.pairs $ fold
  [ Aeson.Encoding.pair "sudo" $ Aeson.Encoding.bool False
  , Aeson.Encoding.pair "language" $ Aeson.Encoding.text "generic"
  -- bors-ng configuration
  , Aeson.Encoding.pair "branches" $ Aeson.Encoding.pairs $
      Aeson.Encoding.pair "only" $ Aeson.Encoding.list Aeson.Encoding.text
      [ "staging"
      , "trying"
      ]
  -- travis emails are really annoying
  , Aeson.Encoding.pair "notifications" $ Aeson.Encoding.pairs $
      Aeson.Encoding.pair "email" $ Aeson.Encoding.bool False
  -- cache v2-build's package stores
  , Aeson.Encoding.pair "cache" $ Aeson.Encoding.pairs $ fold
      [ Aeson.Encoding.pair "directories" $ Aeson.Encoding.list Aeson.Encoding.text
        [ "$HOME/.cabal/store"
        , "$HOME/.cabal/bin"
        ]
      ]
  , Aeson.Encoding.pair "matrix" $ Aeson.Encoding.pairs $ fold
    [ Aeson.Encoding.pair "fast_finish" $ Aeson.Encoding.bool True
    , Aeson.Encoding.pair "allow_failures" $ Aeson.Encoding.list id $
      foldMap (uncurry buildAllowFailures) buildMap
    ]
  , Aeson.Encoding.pair "jobs" $ Aeson.pairs $ Aeson.Encoding.pair "include" $ Aeson.Encoding.list id buildJobs
  ]
  where

    aptPair :: GHCVersion -> Aeson.Encoding.Series
    aptPair ghc =
      Aeson.Encoding.pair "addons" $  Aeson.Encoding.pairs $ Aeson.Encoding.pair "apt" $ Aeson.Encoding.pairs $ fold
        [ Aeson.Encoding.pair "sources" $ Aeson.Encoding.list Aeson.Encoding.text [ "hvr-ghc" ]
        , Aeson.Encoding.pair "packages" $ Aeson.Encoding.list Aeson.Encoding.text
          [ "ghc-" <> ghcAptVersion ghc
          , case ghcRegularity ghc of
              Regular{} -> "cabal-install-3.0"
              PreRelease ver _ -> "cabal-install-" <> ver
          ]
        ]

    buildJobs = flip fmap buildMap $ \ ( buildName, build ) ->
      Aeson.Encoding.pairs $ fold
        [ Aeson.Encoding.pair "env" $ Aeson.Encoding.text $ env buildName
        , aptPair $ buildGHCVersion build
        , Aeson.Encoding.pair "before_install" $ Aeson.Encoding.list Aeson.Encoding.text
          [ "export PATH=/opt/ghc/bin:$PATH"
          ]
        , Aeson.Encoding.pair "install" $ Aeson.Encoding.text "./travis/deps.sh"
        , Aeson.Encoding.pair "script" $ buildScript build
        ]

    buildScript build = Aeson.Encoding.list Aeson.Encoding.text $ fold
      [ [ "./travis/build.sh" ]
      , case buildRunMeta build of
          MetaNo ->
            []
          MetaYes ->
            [ "./travis/meta.sh" ]
      ]

generateProjectFiles :: Map SText Build -> Map FilePath LByteString
generateProjectFiles = ifoldMap $ \ buildName build ->
  -- TODO: https://github.com/haskell/cabal/issues/1597
  let
    dest = addExtension ( "cabal" </> ST.unpack buildName ) "project"
  in
    Map.singleton dest $ BSB.toLazyByteString $ LTEnc.encodeUtf8Builder $ foldMap (<> "\n") $ fold
      [ [ "-- WARNING: This file is automatically generated by 'meta generate'."
        , "packages:"
        ]
      , flip fmap ( buildPackages build ) $ \ package ->
          "  " <> LT.pack ( packageDirectory package )
      , [ "with-compiler: ghc-" <> LT.fromStrict ( ghcAptVersion ( buildGHCVersion build ) )
        -- Needed or else Cabal's autodetection of the ghc-pkg command goes wrong. See haskell/cabal#5792.
        , "with-hc-pkg: ghc-pkg-" <> LT.fromStrict ( ghcAptVersion ( buildGHCVersion build ) )
        , "optimization: False"
        , "benchmarks: true"
        , "tests: true"
        ]
      , case ghcRegularity $ buildGHCVersion build of
          PreRelease{} ->
            [ "repository head.hackage"
            , "  url: http://head.hackage.haskell.org/"
            , "  secure: True"
            , "  root-keys:"
            , "    07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740"
            , "    2e8555dde16ebd8df076f1a8ef13b8f14c66bad8eafefd7d9e37d0ed711821fb"
            , "    8f79fd2389ab2967354407ec852cbe73f2e8635793ac446d09461ffb99527f6e"
            , "  key-threshold: 3"
            , ""
            , "allow-newer: *"
            , ""
            , "keep-going: true"
            , ""
            ]
          Regular{} -> []
      , case buildWError build of
          WErrorNo -> []
          WErrorYes ->
            -- This is a hack around https://github.com/haskell/cabal/issues/3883: we can't specify ghc-options for all local packages only, so instead specify ghc-options for each, individual local package.
            flip foldMap ( buildPackages build ) $ \ package ->
              [ "package " <> LT.pack ( unPackageName ( packageName $ packageGPD package ) )
              , "  ghc-options: -Werror"
              ]
      ]

generateTopLevelProject :: [ Package ] -> LByteString
generateTopLevelProject pkgs =
  BSB.toLazyByteString $ LTEnc.encodeUtf8Builder $ foldMap (<> "\n") $ fold
    [ [ "-- WARNING: This file is automatically generated by 'meta generate'."
      , "packages:"
      ]
    , flip fmap pkgs $ \ package ->
        "  " <> LT.pack ( packageDirectory package )
    , [ ""
      , "optimization: False"
      , "benchmarks: true"
      , "tests: true"
      , ""
      , "constraints:"
      , LTB.toLazyText $ intercalateMap0 ",\n" ( \ constr -> "  " <> PF.renderConstraint constr ) extraConstraints
      ]
    ]

data PackageConfig = PackageConfig
  { packageConfigExcluded :: Set SText
  }

packageConfigSchema
  :: ( Desc tag )
  => El tag PackageConfig
packageConfigSchema
  = rfmap (iso f g) $ elementMixed ( uname "package-config" ) $ flowEvenPreWSDropComments
  $ rcons ( flowWSEDropComments $ elementMixed ( uname "exclude" )
          $ flowEvenPreWSDropComments versions
          )
  $ rnil

  where

    f :: HProd '[ [SText] ] -> PackageConfig
    f ( HProdCons excluded HProdNil ) =
      PackageConfig ( Set.fromList excluded )

    g :: PackageConfig -> HProd '[ [SText] ]
    g ( PackageConfig excluded ) =
      HProdCons ( toList excluded ) HProdNil

    versions :: ( Desc tag ) => EvenFlow tag [SText]
    versions
      = rmany $ flowWSEDropComments
      $ rfmap ( iso LT.toStrict LT.fromStrict )
      $ elementMixed ( uname "build" )
      $ rfmap consolidate
      $ oddTxNoPos

parsePackageConfig :: FilePath -> IO PackageConfig
parsePackageConfig path = do
  (res, warns) <- parseXML' <$> STIO.readFile path
  unless (null warns) $ do
    STIO.hPutStrLn stderr $ "While parsing " <> ST.pack path
    LTIO.hPutStr stderr $ LTB.toLazyText $ displayWarnings warns
  case res of
    Left err -> do
      STIO.hPutStrLn stderr $ "While parsing " <> ST.pack path
      LTIO.hPutStrLn stderr $ LTB.toLazyText $ displayError err
      exitFailure
    Right el ->
      case XMLDesc.Parse.parse packageConfigSchema el of
        Nothing -> do
          STIO.hPutStrLn stderr $ "While parsing " <> ST.pack path
          STIO.hPutStrLn stderr "Extraction failed."
          exitFailure
        Just packageConfig ->
          pure packageConfig

data BuildConfig = BuildConfig
  { _buildConfigGHCVersion :: GHCVersion
  , _buildConfigRunMetaChecks :: RunMetaChecks
  , _buildConfigWError :: WError
  }

configs :: Map SText BuildConfig
configs = Map.fromList
  [ ( "ghc-8.4", BuildConfig GHC8_4 MetaNo WErrorNo )
  , ( "ghc-8.6", BuildConfig GHC8_6 MetaYes WErrorYes )
  , ( "ghc-8.8", BuildConfig GHC8_8 MetaNo WErrorYes )
  , ( "ghc-head", BuildConfig GHCHEAD MetaNo WErrorNo )
  ]

-- The values are the sources of the exclusion.
computeTransitiveExclusions
  :: ( Ord a )
  => ( a -> Set a )
  -> Set a
  -> MonoidalMap a ( Set a )
computeTransitiveExclusions adjacent input =
  execAccum
    ( go ( MonoidalMap.fromSet Set.singleton input ) )
    mempty
  where
  go cur = do
    new <- MonoidalMap.difference cur <$> Accum.look
    Accum.add cur
    if null new
    then pure ()
    else go $ flip ifoldMap new $ \ a deps ->
      MonoidalMap.fromSet ( const deps ) ( adjacent a )

constructBuilds
  :: [ (Package, PackageConfig) ]
  -> Either SText ( Map SText Build )
constructBuilds packageData = do
  let
    packagesByName :: Map PackageName GenericPackageDescription
    packagesByName = Map.fromList $ packageData <&> \ ( Package _ gpd, _ ) -> ( packageName gpd, gpd )
    localDependents :: MonoidalMap GHCVersion ( MonoidalMap PackageName ( Set PackageName ) )
    localDependents = flip ifoldMap packagesByName $ \ name gpd ->
      flip foldMap configs $ \ ( BuildConfig ghcVer _ _ ) ->
        MonoidalMap.singleton ghcVer $
          flip foldMap ( allDependencies ghcVer gpd ) $ \ depName ->
            if Set.member depName local
            then MonoidalMap.singleton depName ( Set.singleton name )
            else mempty
    immediateExclusions :: Map SText ( NonEmpty Package )
    immediateExclusions = getMonoidalMap $
      flip foldMap packageData $ \ ( package, packageConfig ) ->
        flip foldMap ( packageConfigExcluded packageConfig ) $ \ config ->
          MonoidalMap.singleton config $ pure package
    local = Set.fromList $ packageName . packageGPD . fst <$> packageData
    makeBuild ( BuildConfig ghc runMeta werror ) immediatelyExcludedPackages =
      let
        adjacent pkg = lookupMonoidal pkg $ lookupMonoidal ghc localDependents
        transitivelyExcludedPackages :: MonoidalMap PackageName ( Set PackageName )
        transitivelyExcludedPackages = computeTransitiveExclusions
          adjacent
          ( Set.fromList $ packageName . packageGPD <$> immediatelyExcludedPackages )
        isExcluded :: Package -> Bool
        isExcluded pkg = MonoidalMap.member ( packageName . packageGPD $ pkg ) transitivelyExcludedPackages
      in
        Build ghc ( filter ( not . isExcluded ) $ fst <$> packageData ) transitivelyExcludedPackages runMeta werror
  mergeA
    ( mapMissing $ \ _ config -> makeBuild config mempty )
    ( traverseMissing $ \ configName pkgs ->
        Left $ "Unknown config '" <> configName <> "' mentioned in config.xml in " <> intercalateMap0 ", " ( ST.pack . packageDirectory ) pkgs <> "."
    )
    ( zipWithMatched $ \ _ config excl -> makeBuild config ( toList excl ) )
    configs
    immediateExclusions

data Command
  = Generate
  | Refreeze
  | TestedWith TestedWithOptions
  | DefrostTarball DefrostTarballOptions
  | ExplainExclusion ExplainExclusionOptions

-- TODO: move this and ExplainExclusionOptions to take the path to the package directory, rather than the PackageName?
data TestedWithOptions = TestedWithOptions
  { _testedWithPackage :: PackageName
  }

data DefrostTarballOptions = DefrostTarballOptions
  { _defrostTarballInput :: FilePath
  , _defrostTarballTestedWith :: FilePath
  , _defrostTarballCommit :: SText
  , _defrostTarballOutput :: FilePath
  }

data ExplainExclusionOptions = ExplainExclusionOptions
  { _explainExclusionPackageName :: PackageName
  , _explainExclusionBuild :: SText
  }

commandParser :: OA.Parser Command
commandParser = OA.hsubparser $ fold
  [ OA.command "generate" $
      OA.info ( pure Generate ) ( OA.progDesc "Regenerate .travis.yml, cabal.project, and cabal/*.project." )
  , OA.command "refreeze" $
      OA.info ( pure Refreeze ) ( OA.progDesc "Regenerate the freeze files." )
  , OA.command "tested-with" $
      OA.info ( TestedWith <$> testedWithParser ) ( OA.progDesc "Extract the tested-with dependencies for a package from the freeze files." )
  , OA.command "defrost-tarball" $
      OA.info ( DefrostTarball <$> defrostTarballParser ) ( OA.progDesc "Defrost the tarball." )
  , OA.command "explain-exclusion" $
      OA.info ( ExplainExclusion <$> explainExclusionParser ) (OA.progDesc "Explain why a package isn't included in a build." )
  ]

testedWithParser :: OA.Parser TestedWithOptions
testedWithParser = pure TestedWithOptions
  <*> OA.strOption
        ( OA.long "package" )

defrostTarballParser :: OA.Parser DefrostTarballOptions
defrostTarballParser = pure DefrostTarballOptions
  <*> OA.strOption
        ( OA.long "input" )
  <*> OA.strOption
        ( OA.long "tested-with" )
  <*> OA.strOption
        ( OA.long "commit" )
  <*> OA.strOption
        ( OA.long "output" )

explainExclusionParser :: OA.Parser ExplainExclusionOptions
explainExclusionParser = pure ExplainExclusionOptions
  <*> OA.strOption
        ( OA.long "package" )
  <*> OA.strOption
        ( OA.long "build" )

main :: IO ()
main = do
  -- sort so that we get better diffability
  packageDirectories <- List.sort . fmap ("packages" </>) <$> listDirectory "packages"
  packageData <- for packageDirectories $ \ dir -> do
    config <- parsePackageConfig ( dir </> "config.xml" )
    packageDescFile <- tryFindPackageDesc Verbosity.normal dir
    gpd <- do
      input <- BS.readFile packageDescFile
      let
        inputParseRes = parseGenericPackageDescription input
      case runParseResult inputParseRes of
        (_warns, Left (_versionMay, errs)) ->
          fail $ foldMap (showPError $ dir </> packageDescFile) errs
        (_warns, Right gpd) ->
          pure gpd
    pure ( Package dir gpd, config )
  OA.execParser ( OA.info commandParser mempty ) >>= \case
    Generate -> runGenerate packageData
    Refreeze -> runRefreeze packageData
    TestedWith opts -> runTestedWith packageData opts
    DefrostTarball opts -> runDefrostTarball opts
    ExplainExclusion opts -> runExplainExclusion packageData opts

-- TODO: clear out cabal/ and/or create the directory (ah, but we don't want to nuke the freeze files! So just rm cabal/*.project)
runGenerate :: [(Package, PackageConfig)] -> IO ()
runGenerate packageData = do
  case constructBuilds packageData of
    Left err -> do
      STIO.putStrLn err
      exitFailure
    Right builds -> do
      let
        projectFiles = generateProjectFiles builds
        topLevelProjectContents = generateTopLevelProject ( fst <$> packageData )
        -- sort so that the heaviest builds go earlier, for better use of parallelism.
        sortedBuilds :: [(SText, Build)]
        sortedBuilds = List.sortOn (negate . length . buildPackages . snd) $ Map.assocs builds
        travisFileContents = fold
          [ "# ++++++++++++++++++++ WARNING ++++++++++++++++++++\n"
          , "# This file is generated by 'meta generate'. Don't hand-edit.\n"
          , "# ++++++++++++++++++++ WARNING ++++++++++++++++++++\n"
          , Aeson.Encoding.fromEncoding $ travisConfiguration sortedBuilds
          ]
        allFiles = fold
          [ projectFiles
          , Map.singleton "cabal.project" topLevelProjectContents
          , Map.singleton ".travis.yml" $ BSB.toLazyByteString travisFileContents
          ]
      ifor_ allFiles $ \ file contents -> do
        STIO.putStrLn $ "Writing " <> ST.pack file
        LBS.writeFile file contents

extraConstraints :: [ PF.Constraint ]
extraConstraints =
  -- Though it's a boot library, it's not depended on by GHC so we have freedom to reinstall. We need 3.0 because the API changed and we moved to the new one - however, some of the setup-only dependencies are lagging.
  [ PF.constraintVersion "Cabal" PF.unqualifiedOnly $
      orLaterVersion ( mkVersion [ 3, 0, 0, 0 ] )
  ]

runRefreeze :: [(Package, PackageConfig)] -> IO ()
runRefreeze packageData = do
  case constructBuilds packageData of
    Left err -> do
      STIO.putStrLn err
      exitFailure
    Right builds -> do
      let
        buildsWithFreezeFiles
            = builds
            & filter
                ( \ build ->
                    case ghcRegularity $ buildGHCVersion build of
                      PreRelease{} ->
                        False
                      Regular{} ->
                        True
                )
      -- Note: since we're limiting ourselves to non-prereleases, we know that they're not pulling in any extra repos
      callProcess "cabal" [ "v2-update" ]
      refreeze buildsWithFreezeFiles

refreeze :: Map SText Build -> IO ()
refreeze builds = ifor_ builds $ \ buildName _build -> do
  STIO.putStrLn $ "Refreezing " <> buildName <> "."
  let
    projectFile = addExtension ( "cabal" </> ST.unpack buildName ) "project"
    freezeFile = addExtension projectFile "freeze"

  -- Constraints need to be kept out of the project file proper or else reject-unconstrained-dependencies will spot them on Travis and ignore them even if they're not fully frozen.
  LTIO.writeFile ( addExtension projectFile "local" ) $ LTB.toLazyText $ foldMap (<> "\n")
    [ "-- WARNING: This file is automatically generated by 'meta refreeze'. It shouldn't be committed."
    , "constraints:"
    , intercalateMap0 ",\n" ( \ constr -> "  " <> PF.renderConstraint constr ) extraConstraints
    ]
  void $ tryJust
    ( guard . isDoesNotExistError )
    ( removeFile freezeFile )
  -- First, just establish the build plan.
  callProcess "cabal"
    [ "v2-build" , "--project-file", projectFile, "--verbose=0", "--dry-run", "all" ]
  -- Now there's a plan.json file with the build plan in it.
  plan <- Plan.findAndDecodePlanJson ( Plan.ProjectRelativeToDir "." )
  case validationToEither $ Freeze.freezeConstraints plan of
    Left errs -> do
      foldMap ( LTIO.putStr . LTB.toLazyText . Freeze.formatError ) errs
      exitFailure
    Right constraints -> do
      LTIO.writeFile freezeFile $ LTB.toLazyText $ foldMap (<> "\n")
        [ "-- WARNING: This file is automatically generated by 'meta refreeze'. Don't hand-edit it."
        , "constraints:"
        , intercalateMap0 ",\n" ( \ constr -> "  " <> PF.renderConstraint constr ) constraints
        ]
      STIO.putStrLn ( "Freeze file " <> ST.pack freezeFile <> " written." )

-- Note: obviously it's not ideal to break compatibility with the tested-with file's format, but it's not the end of the world if we do. The worst effects are that any releases in progress will have to be scrubbed (unlikely), and that we'll be unable to make revisions for releases before the break - but the fix for that is easy: just cut a new release. As said, that's not ideal, but it's not the end of the world. What we really want is for parsing old versions to fail reliably if there has been an irreconcilable break, rather than silently producing corrupt data (which rules out most binary formats).

-- You might also be wondering whether we could avoid storing the full constraints and instead just have a single version per dependency - but we don't know the relevant versions to use for defrosting a custom-setup until we know the name of the package we're defrosting. This isn't overcomable (since we do obviously know it here), but it complicates defrost's API.

envsForPackage :: [ ( Package, PackageConfig ) ] -> PackageName -> IO [ Defrost.Env ]
envsForPackage packageData pn =
  case constructBuilds packageData of
    Left err -> do
      STIO.putStrLn err
      exitFailure
    Right builds ->
      fmap toList $ flip Map.traverseMaybeWithKey builds $ \ buildName build -> runMaybeT $ do
        let
          ghc = buildGHCVersion build
        guard $ isRegular ghc
        guard $ not $ MonoidalMap.member pn ( buildExcludedPackages build )
        lift $ do
          parseRes <- PF.parse <$> STIO.readFile ( addExtension ( "cabal" </> ST.unpack buildName ) "project.freeze" )
          case parseRes of
            Left err -> do
              STIO.putStrLn $ PF.renderError err
              exitFailure
            Right projectConfig ->
              -- TODO: principled OS, arch, flags
              pure $ Defrost.env
                Distribution.System.Linux
                Distribution.System.X86_64
                mempty
                Compiler.GHC
                ( ghcCompilerVersion ghc )
                ( view PF.constraints projectConfig )

runTestedWith :: [ ( Package, PackageConfig ) ] -> TestedWithOptions -> IO ()
runTestedWith packageData ( TestedWithOptions pn ) = do
  envs <- envsForPackage packageData pn
  LBS.putStr $ Aeson.encode $ toList envs

sourceRepos :: Lens' PD.PackageDescription [ SourceRepo ]
sourceRepos f pd = f ( PD.sourceRepos pd ) <&> \ srs ->
  pd { PD.sourceRepos = srs }

addThisSourceRepo :: SText -> GenericPackageDescription -> GenericPackageDescription
addThisSourceRepo commit = over ( packageDescription . sourceRepos ) $ \ orig ->
  let
    this = flip mapMaybe orig $ \ repo -> do
      guard ( repoKind repo == RepoHead )
      pure $ repo
        { repoKind = RepoThis
        , repoTag = Just $ ST.unpack commit
        }
  in
    orig <> this

defrostTarball
  :: SText
  -> [ Defrost.Env ]
  -> FilePath
  -> FilePath
  -> IO ()
defrostTarball commit envs src dst =
  withSystemTempDirectory "defrost" $ \ tmpDir -> do
    Proc.callProcess "tar"
      [ "--extract", "--file", src, "--strip-components", "1", "--directory", tmpDir ]
    cabalFile <- tryFindPackageDesc Verbosity.normal tmpDir
    input <- BS.readFile cabalFile
    let
      inputParseRes = parseGenericPackageDescription input
    case runParseResult inputParseRes of
      (_warns, Left (_versionMay, errs)) ->
        fail $ foldMap (showPError cabalFile) errs
      (_warns, Right inputGpd) ->
        case Defrost.defrost mempty extraConstraints envs inputGpd of
          Left err -> do
            STIO.putStrLn err
            exitFailure
          Right outputGpd ->
            writeGenericPackageDescription cabalFile $ addThisSourceRepo commit outputGpd
    withFile dst WriteMode $ \ dstHnd -> do
      let
        args = ( Proc.proc "cabal" [ "v2-sdist", "-o", "-" ] )
          { Proc.std_out = Proc.UseHandle dstHnd
          , Proc.cwd = Just tmpDir
          }
      (_, _, _, procHnd) <- Proc.createProcess_ "cabal v2-sdist" args
      res <- Proc.waitForProcess procHnd
      case res of
        ExitSuccess ->
          pure ()
        ExitFailure code -> do
          STIO.putStrLn $
            "v2-sdist failed with exit code" <> ST.pack (show code) <> "."
          exitFailure

runDefrostTarball :: DefrostTarballOptions -> IO ()
runDefrostTarball (DefrostTarballOptions input testedWithFile commit output) =
  Aeson.eitherDecodeFileStrict testedWithFile >>= \case
    Left err -> do
      STIO.putStrLn $ ST.pack err
      exitFailure
    Right testedWith ->
      defrostTarball
        commit
        testedWith
        input
        output

runExplainExclusion
  :: [ ( Package, PackageConfig ) ]
  -> ExplainExclusionOptions
  -> IO ()
runExplainExclusion packageData ( ExplainExclusionOptions pkg buildName ) = do
  case constructBuilds packageData of
    Left err -> do
      STIO.putStrLn err
      exitFailure
    Right builds -> do
      case Map.lookup buildName builds of
        Nothing -> do
          STIO.putStrLn $ fold
            [ "\"", buildName, "\" is not a known build." ]
          exitFailure
        Just build -> do
          let
            blockers = lookupMonoidal pkg ( buildExcludedPackages build )
          if null blockers
          then
            STIO.putStrLn "Not excluded."
          else do
            STIO.putStrLn $ "Exclusion is cascading from:"
            for_ blockers $ \ pkgName ->
              STIO.putStrLn $ fold
                [ "* ", ST.pack ( unPackageName pkgName ) ]
