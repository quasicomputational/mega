-- #######################################################################
-- ############################## ATTENTION ##############################
-- #######################################################################
-- If you are editing this file, make sure to run 'cabal new-run meta gen-travis' afterwards.
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
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder ( hPutBuilder )
import qualified Data.ByteString.Builder as BSB
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map.Merge.Lazy ( dropMissing, mergeA, traverseMissing, zipWithMatched )
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text.Encoding as STEnc
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO
import Distribution.Types.VersionRange
  ( orLaterVersion
  )
import Distribution.Version
  ( mkVersion
  )
import GHC.Real (fromIntegral)
import qualified Hpack
import qualified Options.Applicative as OA
import qualified Q4C12.MapPend as MapPend
import qualified Q4C12.ProjectFile as PF
import Q4C12.XML ( parseXML', displayWarnings, displayError, uname )
import qualified Q4C12.XMLDesc.Parse as XMLDesc.Parse
import Q4C12.XMLDesc ( Desc, El, EvenFlow, consolidate, elementMixed, rfmap, rmany, rcons, rnil, flowWSEDropComments, flowEvenPreWSDropComments, oddTxNoPos )
import System.Process
  ( callProcess )

-- TODO: wouldn't it be cool to integrate this with SUPPORT.markdown, and have a machine-checked policy for older GHC compatibility?

-- There's a certain amount of duplicated work going on when two builds share a GHC version, but downloading from the PPA is SO SLOW that it's worth it; after the first slow runs it gets cached just fine. If the per-container overhead was reduced, then we could move back to aiming for no redundant builds of dependencies, but it's just too much as it is.

data Regularity
  = Regular
  | PreRelease
    { _preReleaseCabal :: SText
    }

data GHCVersion
  = GHC8_4
  | GHC8_6
  | GHC8_8
  | GHCHEAD
  deriving ( Eq, Ord )

-- This is the version that the package name is built from, hence lowercase 'head'.
ghcVersion :: GHCVersion -> SText
ghcVersion = \case
  GHC8_4 -> "8.4.4"
  GHC8_6 -> "8.6.5"
  GHC8_8 -> "8.8.1"
  GHCHEAD -> "head"

ghcRegularity :: GHCVersion -> Regularity
ghcRegularity = \case
  GHC8_4 -> Regular
  GHC8_6 -> Regular
  GHC8_8 -> PreRelease "3.0"
  GHCHEAD -> PreRelease "head"

data RunMetaChecks = MetaNo | MetaYes

data WError = WErrorNo | WErrorYes

data Package = Package
  { packageDirectory :: FilePath
  , packageName :: ST.Text
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
    Regular -> False
    PreRelease _ -> True
  pure $ Aeson.pairs $
    Aeson.pair "env" $ Aeson.text $ env buildName

travisConfiguration :: [(SText, Build)] -> Aeson.Encoding
travisConfiguration buildMap = Aeson.pairs $ fold
  [ Aeson.pair "sudo" $ Aeson.bool False
  , Aeson.pair "language" $ Aeson.text "generic"
  -- bors-ng configuration
  , Aeson.pair "branches" $ Aeson.pairs $
      Aeson.pair "only" $ Aeson.list Aeson.text
      [ "staging"
      , "trying"
      ]
  -- travis emails are really annoying
  , Aeson.pair "notifications" $ Aeson.pairs $
      Aeson.pair "email" $ Aeson.bool False
  -- cache new-build's package stores
  , Aeson.pair "cache" $ Aeson.pairs $ fold
      [ Aeson.pair "directories" $ Aeson.list Aeson.text
        [ "$HOME/.cabal/store"
        , "$HOME/.cabal/bin"
        ]
      ]
  , Aeson.pair "matrix" $ Aeson.pairs $ fold
    [ Aeson.pair "fast_finish" $ Aeson.bool True
    , Aeson.pair "allow_failures" $ Aeson.list id $
      foldMap (uncurry buildAllowFailures) buildMap
    ]
  , Aeson.pair "jobs" $ Aeson.pairs $ Aeson.pair "include" $ Aeson.list id buildJobs
  ]
  where

    aptPair :: GHCVersion -> Aeson.Series
    aptPair ghc =
      Aeson.pair "addons" $  Aeson.pairs $ Aeson.pair "apt" $ Aeson.pairs $ fold
        [ Aeson.pair "sources" $ Aeson.list Aeson.text [ "hvr-ghc" ]
        , Aeson.pair "packages" $ Aeson.list Aeson.text
          [ "ghc-" <> ghcVersion ghc
          , case ghcRegularity ghc of
              Regular -> "cabal-install-2.4"
              PreRelease ver -> "cabal-install-" <> ver
          ]
        ]

    buildJobs = flip fmap buildMap $ \ ( buildName, build ) ->
      Aeson.pairs $ fold
        [ Aeson.pair "env" $ Aeson.text $ env buildName
        , aptPair $ buildGHCVersion build
        , Aeson.pair "before_install" $ Aeson.list Aeson.text
          [ "export PATH=/opt/ghc/bin:$PATH"
          ]
        , Aeson.pair "install" $ Aeson.text "./travis/deps.cabal-new-build.sh"
        , Aeson.pair "script" $ buildScript build
        ]

    buildScript build = Aeson.list Aeson.text $ fold
      [ [ "./travis/build.cabal-new-build.sh" ]
      , case buildRunMeta build of
          MetaNo ->
            []
          MetaYes ->
            [ "./travis/meta.cabal-new-build.sh" ]
      ]

generateProjectFiles :: Map SText Build -> IO ()
generateProjectFiles = traverseWithKey_ $ \ buildName build -> do
  -- TODO: https://github.com/haskell/cabal/issues/1597
  -- TODO: clear out cabal/ and/or create the directory (ah, but we don't want to nuke the freeze files! So just rm cabal/*.project)
  let dest = addExtension ( "cabal" </> ST.unpack buildName ) "project"
  STIO.putStrLn $ "Writing file " <> ST.pack dest
  LTIO.writeFile dest $ foldMap (<> "\n") $ fold
    [ [ "-- WARNING: This file is automatically generated by 'meta gen-travis'."
      , "packages:"
      ]
    , flip fmap ( buildPackages build ) $ \ package ->
        "  " <> LT.pack ( packageDirectory package )
    , [ "with-compiler: ghc-" <> LT.fromStrict ( ghcVersion ( buildGHCVersion build ) )
      -- Needed or else Cabal's autodetection of the ghc-pkg command goes wrong. See haskell/cabal#5792.
      , "with-hc-pkg: ghc-pkg-" <> LT.fromStrict ( ghcVersion ( buildGHCVersion build ) )
      , "optimization: False"
      , "benchmarks: true"
      , "tests: true"
      ]
    , case ghcRegularity $ buildGHCVersion build of
        PreRelease _ ->
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
          ]
        Regular -> []
    , [ "constraints:" ]
    , flip fmap extraConstraints $ \ constraint ->
        LTB.toLazyText $ "  " <> PF.renderConstraint constraint
    , case buildWError build of
        WErrorNo -> []
        WErrorYes ->
          -- This is a hack around https://github.com/haskell/cabal/issues/3883: we can't specify ghc-options for all local packages only, so instead specify ghc-options for each, individual local package.
          flip foldMap ( buildPackages build ) $ \ package ->
            [ "package " <> LT.fromStrict ( packageName package )
            , "  ghc-options: -Werror"
            ]
    ]
  where
    --TODO: https://github.com/haskell/containers/issues/422
    traverseWithKey_ :: ( Applicative f ) => ( k -> a -> f b ) -> Map k a -> f ()
    traverseWithKey_ f = void . Map.traverseWithKey f

data PackageConfig = PackageConfig
  { packageConfigExcluded :: Set SText
  , packageConfigIncluded :: Set SText
  }

packageConfigSchema
  :: ( Desc tag )
  => El tag PackageConfig
packageConfigSchema
  = rfmap (iso f g) $ elementMixed ( uname "package-config" ) $ flowEvenPreWSDropComments
  $ rcons ( flowWSEDropComments $ elementMixed ( uname "exclude" )
          $ flowEvenPreWSDropComments versions
          )
  $ rcons ( flowWSEDropComments $ elementMixed ( uname "include" )
          $ flowEvenPreWSDropComments versions
          )
  $ rnil

  where

    f :: HProd '[ [SText], [SText] ] -> PackageConfig
    f ( HProdCons excluded ( HProdCons included HProdNil ) ) =
      PackageConfig ( Set.fromList excluded ) ( Set.fromList included )

    g :: PackageConfig -> HProd '[ [SText], [SText] ]
    g ( PackageConfig excluded included ) =
      HProdCons ( toList excluded ) $ HProdCons ( toList included ) $ HProdNil

    versions :: ( Desc tag ) => EvenFlow tag [SText]
    versions
      = rmany $ flowWSEDropComments
      $ rfmap ( iso LT.toStrict LT.fromStrict )
      $ elementMixed ( uname "build" )
      $ rfmap consolidate
      $ oddTxNoPos

parsePackageConfig :: FilePath -> IO PackageConfig
parsePackageConfig path = do
  STIO.putStrLn $ "Parsing " <> ST.pack path
  (res, warns) <- parseXML' <$> STIO.readFile path
  LTIO.putStr $ LTB.toLazyText $ displayWarnings warns
  case res of
    Left err -> do
      LTIO.putStrLn $ LTB.toLazyText $ displayError err
      exitFailure
    Right el ->
      case XMLDesc.Parse.parse packageConfigSchema el of
        Nothing -> do
          STIO.putStrLn "Extraction failed."
          exitFailure
        Just packageConfig ->
          pure packageConfig

data Optability = OptIn | OptOut

data BuildConfig = BuildConfig
  { _buildConfigGHCVersion :: GHCVersion
  , _buildConfigOptability :: Optability
  , _buildConfigRunMetaChecks :: RunMetaChecks
  , _buildConfigWError :: WError
  }

configs :: Map SText BuildConfig
configs = Map.fromList
  [ ( "ghc-8.4", BuildConfig GHC8_4 OptOut MetaNo WErrorNo )
  , ( "ghc-8.6", BuildConfig GHC8_6 OptOut MetaYes WErrorYes )
  , ( "ghc-8.8", BuildConfig GHC8_8 OptOut MetaNo WErrorYes )
  , ( "ghc-head", BuildConfig GHCHEAD OptOut MetaNo WErrorNo )
  ]

constructBuilds
  :: [ (Package, PackageConfig) ]
  -> Either SText ( Map SText Build )
constructBuilds packageData = do
  let
    allMentionedConfigs = MapPend.getMapPend $
      flip foldMap packageData $ \ ( package, packageConfig ) -> fold
        [ flip foldMap ( packageConfigIncluded packageConfig ) $ \ config ->
            MapPend.singleton config $ Set.singleton $ packageDirectory package
        , flip foldMap ( packageConfigExcluded packageConfig ) $ \ config ->
            MapPend.singleton config $ Set.singleton $ packageDirectory package
        ]
  --TODO: ask for a mergeA_?
  void $ mergeA
    dropMissing
    ( traverseMissing $ \ configName dirs ->
        Left $ "Unknown config '" <> configName <> "' mentioned in config.xml in " <> intercalateMap0 ", " ST.pack dirs <> "."
    )
    ( zipWithMatched $ \ _ _ _ -> () )
    configs
    allMentionedConfigs
  pure $ flip Map.mapWithKey configs $ \ configName ( BuildConfig ghc opt runMeta werror ) ->
    let packages = flip mapMaybe packageData $ \ ( package, packageConfig ) ->
          if | not ( Set.member configName ( packageConfigIncluded packageConfig ) )
             , OptIn <- opt
             -> Nothing
             | Set.member configName ( packageConfigExcluded packageConfig )
             -> Nothing
             | otherwise
             -> Just package
    in Build ghc packages runMeta werror

generateHash :: [(Package, PackageConfig)] -> BS.ByteString
generateHash = SHA256.hash . LBS.toStrict . BSB.toLazyByteString . foldMap (uncurry serialise)
  where
    -- fromIntegral is scary, but we have no choice.
    setLength :: Set a -> Word64
    setLength = fromIntegral . length

    serialise :: Package -> PackageConfig -> BSBuilder
    serialise ( Package path name ) ( PackageConfig incl excl ) = fold
      [ BSB.stringUtf8 path
      , BSB.word8 0
      , STEnc.encodeUtf8Builder name
      , BSB.word8 0
      , BSB.word64BE $ setLength incl
      , flip foldMap incl $ \ buildName -> fold
          [ STEnc.encodeUtf8Builder buildName
          , BSB.word8 0
          ]
      , BSB.word64BE $ setLength excl
      , flip foldMap excl $ \ buildName -> fold
          [ STEnc.encodeUtf8Builder buildName
          , BSB.word8 0
          ]
      ]

data Command
  = GenTravis
  | CheckHash
  | Refreeze
  | CheckStaleCabal

commandParser :: OA.Parser Command
commandParser = OA.hsubparser $ fold
  [ OA.command "gen-travis" $
      OA.info ( pure GenTravis ) ( OA.progDesc "Regenerate .travis.yml and cabal/*.project." )
  , OA.command "check-hash" $
      OA.info ( pure CheckHash ) ( OA.progDesc "Check that gen-travis has been run recently." )
  , OA.command "refreeze" $
      OA.info ( pure Refreeze ) ( OA.progDesc "Regenerate the freeze files." )
  , OA.command "check-stale-cabal" $
      OA.info ( pure CheckStaleCabal ) ( OA.progDesc "Make sure none of the .cabal files are stale." )
  ]

main :: IO ()
main = do
  -- sort so that we get better diffability
  packageDirectories <- List.sort . fmap ("packages" </>) <$> listDirectory "packages"
  packageData <- for packageDirectories $ \ dir -> do
    config <- parsePackageConfig ( dir </> "config.xml" )
    name <- do
      files <- listDirectory dir
      let
        candidates = mapMaybe ( stripExtension "cabal" ) files
      case candidates of
        [] -> do
          LTIO.putStrLn $
            "No .cabal file found in " <> LT.pack dir <> "."
          exitFailure
        [ name ] ->
          pure $ ST.pack name
        _ -> do
          LTIO.putStrLn $
            "Multiple .cabal files found in " <> LT.pack dir <> "."
          exitFailure
    pure ( Package dir name, config )
  OA.execParser ( OA.info commandParser mempty ) >>= \case
    GenTravis -> runGenTravis packageData
    CheckHash -> runCheckHash packageData
    Refreeze -> runRefreeze packageData
    CheckStaleCabal -> runCheckStaleCabal packageData

runGenTravis :: [(Package, PackageConfig)] -> IO ()
runGenTravis packageData = do
  case constructBuilds packageData of
    Left err -> do
      STIO.putStrLn err
      exitFailure
    Right builds -> do
      let
        -- sort so that the heaviest builds go earlier, for better use of parallelism.
        sortedBuilds :: [(SText, Build)]
        sortedBuilds = List.sortOn (negate . length . buildPackages . snd) $ Map.assocs builds
      withFile ".travis.yml" WriteMode $ \ hnd -> do
        hPutBuilder hnd "# ++++++++++++++++++++ WARNING ++++++++++++++++++++\n"
        hPutBuilder hnd "# This file is generated by 'meta gen-travis'. Don't hand-edit.\n"
        hPutBuilder hnd "# ++++++++++++++++++++ WARNING ++++++++++++++++++++\n"
        hPutBuilder hnd $ Aeson.fromEncoding $ travisConfiguration sortedBuilds
        STIO.putStrLn "Written .travis.yml."
      generateProjectFiles builds
      BS.writeFile "travis/hash" $ generateHash packageData
      STIO.putStrLn "Written hash."

runCheckHash :: [(Package, PackageConfig)] -> IO ()
runCheckHash packageData = do
  let
    expectedHash = generateHash packageData
  actualHash <- BS.readFile "travis/hash"
  unless ( expectedHash == actualHash ) $ do
    STIO.putStrLn "./travis/hash and the computed hash do not match. Do 'cabal new-run meta gen-travis' and make sure all of the files it produces are committed."
    exitFailure

extraConstraints :: [ PF.Constraint ]
extraConstraints =
  -- Needs to be included because it's a boot library, though it's not depended on by GHC so we have freedom to reinstall.
  [ PF.constraintVersion "Cabal" PF.qualifiedAll $
      orLaterVersion ( mkVersion [ 2, 2, 0, 1 ] )
  ]

runRefreeze :: [(Package, PackageConfig)] -> IO ()
runRefreeze packageData = do
  runCheckHash packageData
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
                      PreRelease _ ->
                        False
                      Regular ->
                        True
                )
      -- Note: since we're limiting ourselves to non-prereleases, we know that they're not pulling in any extra repos
      callProcess "cabal" [ "new-update" ]
      refreeze buildsWithFreezeFiles

refreeze :: Map SText Build -> IO ()
refreeze builds = void $ flip Map.traverseWithKey builds $ \ buildName _build -> do
  STIO.putStrLn $ "Refreezing " <> buildName <> "."
  let
    projectFile = addExtension ( "cabal" </> ST.unpack buildName ) "project"

  void $ tryJust
    ( guard . isDoesNotExistError )
    ( removeFile ( addExtension projectFile "freeze" ) )
  callProcess "cabal"
    [ "new-freeze" , "--project-file", projectFile ]

runCheckStaleCabal :: [(Package, PackageConfig)] -> IO ()
runCheckStaleCabal = traverse_ $ \ (package, _) -> do
  let
    packageYaml = packageDirectory package </> "package.yaml"
    options = Hpack.setTarget packageYaml Hpack.defaultOptions
  packageYamlExists <- doesFileExist packageYaml
  when packageYamlExists $ do
    res <- Hpack.hpackResult options
    case Hpack.resultStatus res of
      Hpack.OutputUnchanged ->
        pure ()
      _ -> do
        LTIO.putStrLn $ LT.pack packageYaml <> " has been modified but the corresponding .cabal files has not been regenerated and checked in."
        exitFailure
