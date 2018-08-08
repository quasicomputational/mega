{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Q4C12.GenTravis
  ( main
  )
  where

import qualified Data.Aeson.Encoding as Aeson
import Data.ByteString.Builder ( hPutBuilder )
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map.Merge.Lazy ( dropMissing, mergeA, traverseMissing, zipWithMatched )
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.IO as LTIO
import qualified Q4C12.MapPend as MapPend
import Q4C12.XML ( parseXML', displayWarnings, displayError, uname, Content, contentText, getContent )
import qualified Q4C12.XMLDesc.Parse as XMLDesc.Parse
import Q4C12.XMLDesc ( Desc, El, EvenFlow, elementMixed, rfmap, rmany, rcons, rnil, flowWSEDropComments, flowEvenPreWSDropComments, oddTxNoPos )

-- TODO: wouldn't it be cool to integrate this with SUPPORT.markdown, and have a machine-checked policy for older GHC compatibility?

-- There's a certain amount of duplicated work going on when two builds share a GHC version, but downloading from the PPA is SO SLOW that it's worth it; after the first slow runs it gets cached just fine. If the per-container overhead was reduced, then we could move back to aiming for no redundant builds of dependencies, but it's just too much as it is.

data Regularity = Regular | PreRelease

data GHCVersion
  = GHC8_0
  | GHC8_2
  | GHC8_4
  | GHC8_6
  | GHCHEAD
  deriving ( Eq, Ord )

-- This is the version that the package name is build from, hence lowercase 'head'.
ghcVersion :: GHCVersion -> SText
ghcVersion = \case
  GHC8_0 -> "8.0.2"
  GHC8_2 -> "8.2.2"
  GHC8_4 -> "8.4.3"
  GHC8_6 -> "8.6.1"
  GHCHEAD -> "head"

ghcRegularity :: GHCVersion -> Regularity
ghcRegularity = \case
  GHC8_0 -> Regular
  GHC8_2 -> Regular
  GHC8_4 -> Regular
  GHC8_6 -> PreRelease
  GHCHEAD -> PreRelease

data Build = Build
  { buildGHCVersion :: GHCVersion
  , buildPackages :: [FilePath]
  }

--TODO: use formatting??
env :: SText -> GHCVersion -> SText
env buildName ghc = fold
  [ "CMD=cabal-new-build"
  , " "
  , "GHCVER="
  , ghcVersion ghc
  , " "
  , "PROJECT="
  , buildName
  ]

beforeInstallNewBuild :: Aeson.Series
beforeInstallNewBuild = Aeson.pair "before_install" $ Aeson.list Aeson.text
  [ "mkdir -p \"$HOME/.local/bin\""
  , "export PATH=/opt/ghc/bin:$PATH"
  , "HPACKVER=0.28.2 ./travis/hpack-setup.sh"
  ]

--TODO: pin stack version?
beforeInstallStack :: Aeson.Series
beforeInstallStack = Aeson.pair "before_install" $ Aeson.list Aeson.text
  [ "mkdir -p \"$HOME/.local/bin\""
  , "export PATH=$HOME/.local/bin:/opt/ghc/bin:$PATH"
  , "curl -sL https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C \"$HOME\"/.local/bin '*/stack'"
  , "stack setup"
  ]

buildAllowFailures :: SText -> Build -> [Aeson.Encoding]
buildAllowFailures buildName build = do
  guard $ case ghcRegularity ( buildGHCVersion build ) of
    Regular -> False
    PreRelease -> True
  pure $ Aeson.pairs $
    Aeson.pair "env" $ Aeson.text $ env buildName ( buildGHCVersion build )

travisConfiguration :: Map SText Build -> Aeson.Encoding
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
  -- cache stack and new-build's package stores
  , Aeson.pair "cache" $ Aeson.pairs $
      Aeson.pair "directories" $ Aeson.list Aeson.text
      [ "$HOME/.cabal/store"
      , "$HOME/.cabal/bin"
      , "$HOME/.stack/bin"
      , "$HOME/.stack/precompiled"
      , "$HOME/.stack/programs"
      , "$HOME/.stack/setup-exe-cache"
      , "$HOME/.stack/snapshots"
      ]
  , Aeson.pair "matrix" $ Aeson.pairs $ fold
    [ Aeson.pair "fast_finish" $ Aeson.bool True
    , Aeson.pair "allow_failures" $ Aeson.list id $ fold
      [ [ Aeson.pairs $ Aeson.pair "env" $ Aeson.text "CMD=stack-nightly" ]
      , Map.foldMapWithKey buildAllowFailures buildMap
      ]
    ]
  , Aeson.pair "jobs" $ Aeson.pairs $ Aeson.pair "include" $ Aeson.list id $ fold
    [ [ Aeson.pairs $ fold
        [ Aeson.pair "env" $ Aeson.text "CMD=stack-werror"
        , Aeson.pair "install" $ Aeson.text "./travis/deps.stack.sh" 
        , Aeson.pair "script" $ Aeson.text "./travis/build.stack.sh --ghc-options=-Werror"
        , beforeInstallStack
        ]
      , Aeson.pairs $ fold
        [ Aeson.pair "env" $ Aeson.text "CMD=stack-nightly"
        , Aeson.pair "install" $ Aeson.text "./travis/deps.stack.sh --resolver nightly"
        , Aeson.pair "script" $ Aeson.text "./travis/build.stack.sh --resolver nightly"
        , beforeInstallStack
        ]
      ]
    , buildJobs
    ]
  ]
  where

    aptPair :: GHCVersion -> Aeson.Series
    aptPair ghc =
      Aeson.pair "addons" $  Aeson.pairs $ Aeson.pair "apt" $ Aeson.pairs $ fold
        [ Aeson.pair "sources" $ Aeson.list Aeson.text [ "hvr-ghc" ]
        , Aeson.pair "packages" $ Aeson.list Aeson.text
          [ "ghc-" <> ghcVersion ghc
          , case ghcRegularity ghc of
              Regular -> "cabal-install-2.2"
              PreRelease -> "cabal-install-head"
          ]
        ]

    buildJobs = flip fmap ( Map.assocs buildMap ) $ \ ( buildName, build ) ->
      Aeson.pairs $ fold
        [ Aeson.pair "env" $ Aeson.text $ env buildName $ buildGHCVersion build
        , aptPair $ buildGHCVersion build
        , Aeson.pair "install" $ Aeson.text "./travis/deps.cabal-new-build.sh"
        , Aeson.pair "script" $ Aeson.text "./travis/build.cabal-new-build.sh"
        , beforeInstallNewBuild
        ]

generateProjectFiles :: Map SText Build -> IO ()
generateProjectFiles = traverseWithKey_ $ \ buildName build -> do
  -- TODO: https://github.com/haskell/cabal/issues/1597
  -- TODO: clear out cabal/ and/or create the directory (ah, but we don't want to nuke the freeze files! So just rm cabal/*.project)
  let dest = addExtension ( "cabal" </> ST.unpack buildName ) "project"
  STIO.putStrLn $ "Writing file " <> ST.pack dest
  LTIO.writeFile dest $ foldMap (<> "\n") $ fold
    [ [ "-- WARNING: This file is automatically generated by gen-travis."
      , "packages:"
      ]
    , flip fmap ( buildPackages build ) $ \ package ->
        "  " <> LT.pack package
    , [ "with-compiler: ghc-" <> LT.fromStrict ( ghcVersion ( buildGHCVersion build ) ) ]
    , if | PreRelease <- ghcRegularity $ buildGHCVersion build
         -> [ "repository head.hackage"
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
         | otherwise
         -> []
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

    -- TODO: should this go into Q4C12.XMLDesc?
    consolidate :: Iso (Content cmt pos) (Content cmt' ()) LText LText
    consolidate = iso (foldMap (foldMap snd) . getContent) contentText

parsePackageConfig :: FilePath -> IO PackageConfig
parsePackageConfig path = do
  STIO.putStrLn $ "Parsing " <> ST.pack path
  (res, warns) <- parseXML' <$> STIO.readFile path
  LTIO.putStr $ TBuilder.toLazyText $ displayWarnings warns
  case res of
    Left err -> do
      LTIO.putStrLn $ TBuilder.toLazyText $ displayError err
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
  }

configs :: Map SText BuildConfig
configs = Map.fromList
  [ ( "ghc-8.0", BuildConfig GHC8_0 OptOut )
  , ( "ghc-8.2", BuildConfig GHC8_2 OptOut )
  , ( "ghc-8.4", BuildConfig GHC8_4 OptOut )
  -- TODO: regularise 8.6
  , ( "ghc-8.6", BuildConfig GHC8_6 OptOut )
  , ( "ghc-head", BuildConfig GHCHEAD OptOut )
  ]

constructBuilds
  :: [ (FilePath, PackageConfig) ]
  -> Either SText ( Map SText Build )
constructBuilds packageData = do
  let allMentionedConfigs = MapPend.getMapPend $
        flip foldMap packageData $ \ ( dir, packageConfig ) -> fold
          [ flip foldMap ( packageConfigIncluded packageConfig ) $ \ config ->
              MapPend.singleton config $ Set.singleton dir
          , flip foldMap ( packageConfigExcluded packageConfig ) $ \ config ->
              MapPend.singleton config $ Set.singleton dir
          ]
  --TODO: ask for a mergeA_?
  void $ mergeA
    dropMissing
    ( traverseMissing $ \ configName dirs ->
        --TODO: use formatting?
        Left $ "Unknown config '" <> configName <> "' mentioned in config.xml in " <> intercalateMap0 ", " ST.pack dirs <> "."
    )
    ( zipWithMatched $ \ _ _ _ -> () )
    configs
    allMentionedConfigs
  pure $ flip Map.mapWithKey configs $ \ configName ( BuildConfig ghc opt ) ->
    let packages = flip mapMaybe packageData $ \ ( dir, packageConfig ) ->
          if | not ( Set.member configName ( packageConfigIncluded packageConfig ) )
             , OptIn <- opt
             -> Nothing
             | Set.member configName ( packageConfigExcluded packageConfig )
             -> Nothing
             | otherwise
             -> Just dir
    in Build ghc packages

main :: IO ()
main = do
  -- sort so that we get better diffability
  packageDirectories <- List.sort . fmap ("packages" </>) <$> listDirectory "packages"
  packageData <- for packageDirectories $ \ dir -> do
    config <- parsePackageConfig ( dir </> "config.xml" )
    pure ( dir, config )
  case constructBuilds packageData of
    Left err -> do
      STIO.putStrLn err
      exitFailure
    Right builds -> do
      withFile ".travis.yml" WriteMode $ \ hnd -> do
        hPutBuilder hnd "# ++++++++++++++++++++ WARNING ++++++++++++++++++++\n"
        hPutBuilder hnd "# This file is generated by gen-travis. Don't hand-edit.\n"
        hPutBuilder hnd "# ++++++++++++++++++++ WARNING ++++++++++++++++++++\n"
        hPutBuilder hnd $ Aeson.fromEncoding $ travisConfiguration builds
      generateProjectFiles builds
