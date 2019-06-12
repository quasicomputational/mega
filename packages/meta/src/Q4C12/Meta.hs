-- #######################################################################
-- ############################## ATTENTION ##############################
-- #######################################################################
-- If you are editing this file, make sure to run 'cabal v2-run meta gen-travis' afterwards.
-- #######################################################################
-- ############################## ATTENTION ##############################
-- #######################################################################
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Q4C12.Meta
  ( main
  )
  where

import Control.Exception
  ( tryJust )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MonoidalMap
import Data.Map.Merge.Lazy ( dropMissing, mergeA, traverseMissing, zipWithMatched )
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Encoding as LTEnc
import qualified Data.Text.Lazy.IO as LTIO
import qualified Distribution.Compiler as Compiler
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription
  , runParseResult
  )
import Distribution.PackageDescription.PrettyPrint
  ( writeGenericPackageDescription
  )
import Distribution.Parsec.Common
  ( showPError
  )
import Distribution.Simple.Utils
  ( tryFindPackageDesc
  )
import qualified Distribution.System
import Distribution.Types.GenericPackageDescription
  ( GenericPackageDescription
  )
import Distribution.Types.GenericPackageDescription.Lens
  ( packageDescription
  )
import qualified Distribution.Types.PackageDescription as PD
import Distribution.Types.PackageName
  ( PackageName
  , mkPackageName
  , unPackageName
  )
import Distribution.Types.SourceRepo
  ( SourceRepo ( repoKind, repoTag )
  , RepoKind ( RepoHead, RepoThis )
  )
import Distribution.Types.VersionRange
  ( orLaterVersion
  )
import Distribution.Version
  ( Version
  , mkVersion
  )
import qualified Options.Applicative as OA
import qualified Q4C12.Defrost as Defrost
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

ghcCompilerVersion :: GHCVersion -> Maybe Version
ghcCompilerVersion ghc = case (ghcRegularity ghc) of
  Regular ver -> Just ver
  PreRelease{} -> Nothing

ghcRegularity :: GHCVersion -> Regularity
ghcRegularity = \case
  GHC8_4 -> Regular (mkVersion [8, 4])
  GHC8_6 -> Regular (mkVersion [8, 6])
  GHC8_8 -> PreRelease "3.0"
  GHCHEAD -> PreRelease "head"

data RunMetaChecks = MetaNo | MetaYes

data WError = WErrorNo | WErrorYes

data Package = Package
  { packageDirectory :: FilePath
  , packageName :: PackageName
  }

data Build = Build
  { buildGHCVersion :: GHCVersion
  , buildPackages :: [Package]
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
              Regular{} -> "cabal-install-2.4"
              PreRelease ver -> "cabal-install-" <> ver
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
generateProjectFiles = Map.foldMapWithKey $ \ buildName build ->
  -- TODO: https://github.com/haskell/cabal/issues/1597
  let
    dest = addExtension ( "cabal" </> ST.unpack buildName ) "project"
  in
    Map.singleton dest $ BSB.toLazyByteString $ LTEnc.encodeUtf8Builder $ foldMap (<> "\n") $ fold
      [ [ "-- WARNING: This file is automatically generated by 'meta gen-travis'."
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
              [ "package " <> LT.pack ( unPackageName ( packageName package ) )
              , "  ghc-options: -Werror"
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

constructBuilds
  :: [ (Package, PackageConfig) ]
  -> Either SText ( Map SText Build )
constructBuilds packageData = do
  let
    allMentionedConfigs = getMonoidalMap $
      flip foldMap packageData $ \ ( package, packageConfig ) ->
        flip foldMap ( packageConfigExcluded packageConfig ) $ \ config ->
          MonoidalMap.singleton config $ Set.singleton $ packageDirectory package
  --TODO: ask for a mergeA_?
  void $ mergeA
    dropMissing
    ( traverseMissing $ \ configName dirs ->
        Left $ "Unknown config '" <> configName <> "' mentioned in config.xml in " <> intercalateMap0 ", " ST.pack dirs <> "."
    )
    ( zipWithMatched $ \ _ _ _ -> () )
    configs
    allMentionedConfigs
  pure $ flip Map.mapWithKey configs $ \ configName ( BuildConfig ghc runMeta werror ) ->
    let packages = flip mapMaybe packageData $ \ ( package, packageConfig ) -> do
          guard $ not $ Set.member configName ( packageConfigExcluded packageConfig )
          pure package
    in Build ghc packages runMeta werror

data Command
  = GenTravis
  | Refreeze
  | TestedWith TestedWithOptions
  | DefrostTarball DefrostTarballOptions

data TestedWithOptions = TestedWithOptions
  { _testedWithPackage :: SText
  }

data DefrostTarballOptions = DefrostTarballOptions
  { _defrostTarballInput :: FilePath
  , _defrostTarballTestedWith :: FilePath
  , _defrostTarballCommit :: SText
  , _defrostTarballOutput :: FilePath
  }

commandParser :: OA.Parser Command
commandParser = OA.hsubparser $ fold
  [ OA.command "gen-travis" $
      OA.info ( pure GenTravis ) ( OA.progDesc "Regenerate .travis.yml and cabal/*.project." )
  , OA.command "refreeze" $
      OA.info ( pure Refreeze ) ( OA.progDesc "Regenerate the freeze files." )
  , OA.command "tested-with" $
      OA.info ( TestedWith <$> testedWithParser ) ( OA.progDesc "Extract the tested-with dependencies for a package from the freeze files." )
  , OA.command "defrost-tarball" $
      OA.info ( DefrostTarball <$> defrostTarballParser ) ( OA.progDesc "Defrost the tarball." )
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

main :: IO ()
main = do
  -- sort so that we get better diffability
  packageDirectories <- List.sort . fmap ("packages" </>) <$> listDirectory "packages"
  packageData <- for packageDirectories $ \ dir -> do
    config <- parsePackageConfig ( dir </> "config.xml" )
    name <- mkPackageName . takeBaseName <$> tryFindPackageDesc dir
    pure ( Package dir name, config )
  OA.execParser ( OA.info commandParser mempty ) >>= \case
    GenTravis -> runGenTravis packageData
    Refreeze -> runRefreeze packageData
    TestedWith opts -> runTestedWith opts
    DefrostTarball opts -> runDefrostTarball opts

filesToWrite :: Map SText Build -> Map FilePath LByteString
filesToWrite builds =
  let
    projectFiles = generateProjectFiles builds
    -- sort so that the heaviest builds go earlier, for better use of parallelism.
    sortedBuilds :: [(SText, Build)]
    sortedBuilds = List.sortOn (negate . length . buildPackages . snd) $ Map.assocs builds
    travisFileContents = fold
      [ "# ++++++++++++++++++++ WARNING ++++++++++++++++++++\n"
      , "# This file is generated by 'meta gen-travis'. Don't hand-edit.\n"
      , "# ++++++++++++++++++++ WARNING ++++++++++++++++++++\n"
      , Aeson.Encoding.fromEncoding $ travisConfiguration sortedBuilds
      ]
  in fold
    [ projectFiles
    , Map.singleton ".travis.yml" $ BSB.toLazyByteString travisFileContents
    ]

-- TODO: clear out cabal/ and/or create the directory (ah, but we don't want to nuke the freeze files! So just rm cabal/*.project)
runGenTravis :: [(Package, PackageConfig)] -> IO ()
runGenTravis packageData = do
  case constructBuilds packageData of
    Left err -> do
      STIO.putStrLn err
      exitFailure
    Right builds -> do
      let
        allFiles = filesToWrite builds
      flip traverseWithKey_ allFiles $ \ file contents -> do
        STIO.putStrLn $ "Writing " <> ST.pack file
        LBS.writeFile file contents

-- TODO: https://github.com/haskell/containers/issues/422
traverseWithKey_ :: ( Applicative f ) => ( k -> a -> f b ) -> Map k a -> f ()
traverseWithKey_ f = void . Map.traverseWithKey f

extraConstraints :: [ PF.Constraint ]
extraConstraints =
  -- Needs to be included because it's a boot library, though it's not depended on by GHC so we have freedom to reinstall.
  [ PF.constraintVersion "Cabal" PF.qualifiedAll $
      orLaterVersion ( mkVersion [ 2, 2, 0, 1 ] )
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
            & Map.filter
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
refreeze builds = flip traverseWithKey_ builds $ \ buildName _build -> do
  STIO.putStrLn $ "Refreezing " <> buildName <> "."
  let
    projectFile = addExtension ( "cabal" </> ST.unpack buildName ) "project"

  -- Constraints need to be kept out of the project file proper or else reject-unconstrained-dependencies will spot them on Travis and ignore them even if they're not fully frozen.
  LTIO.writeFile ( addExtension projectFile "local" ) $ LTB.toLazyText $ foldMap (<> "\n")
    [ "-- WARNING: This file is automatically generated by 'meta refreeze'. It shouldn't be committed."
    , "constraints:"
    , intercalateMap0 ",\n" ( \ constr -> "  " <> PF.renderConstraint constr ) extraConstraints
    ]
  void $ tryJust
    ( guard . isDoesNotExistError )
    ( removeFile ( addExtension projectFile "freeze" ) )
  callProcess "cabal"
    [ "v2-freeze" , "--project-file", projectFile ]

-- Note: obviously it's not ideal to break compatibility with the tested-with file's format, but it's not the end of the world if we do. The worst effects are that any releases in progress will have to be scrubbed (unlikely), and that we'll be unable to make revisions for releases before the break - but the fix for that is easy: just cut a new release. As said, that's not ideal, but it's not the end of the world. What we really want is for parsing old versions to fail reliably if there has been an irreconcilable break, rather than silently producing corrupt data (which rules out most binary formats).

-- You might also be wondering whether we could avoid storing the full constraints and instead just have a single version per dependency - but we don't know the relevant versions to use for defrosting a custom-setup until we know the name of the package we're defrosting. This isn't overcomable (since we do obviously know it here), but it complicates defrost's API.

envsForPackageConfig :: PackageConfig -> IO [ Defrost.Env ]
envsForPackageConfig packageConfig =
  fmap toList $ flip Map.traverseMaybeWithKey configs $ \ buildName (BuildConfig ghc _ _) -> runMaybeT $ do
    ghcVer <- hoistMaybe $ ghcCompilerVersion ghc
    guard $ not $ Set.member buildName (packageConfigExcluded packageConfig)
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
            ghcVer
            ( view PF.constraints projectConfig )

runTestedWith :: TestedWithOptions -> IO ()
runTestedWith (TestedWithOptions pkg) = do
  let
    packageDir = "packages" </> ST.unpack pkg

  packageConfig <- parsePackageConfig ( packageDir </> "config.xml" )
  envs <- envsForPackageConfig packageConfig
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
    cabalFile <- tryFindPackageDesc tmpDir
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
