module Main
  ( main
  )
  where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as ST
import qualified Distribution.Compiler as Compiler
import Distribution.PackageDescription
  ( mkFlagAssignment
  )
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription
  , runParseResult
  )
import Distribution.Parsec
  ( showPError
  )
import qualified Distribution.System as System
import Distribution.Types.PackageName
  ( PackageName
  )
import Distribution.Version
  ( mkVersion
  , thisVersion
  , laterVersion
  )
import qualified Q4C12.ProjectFile as PF
import Test.Tasty
  ( TestTree
  , defaultMain
  , testGroup
  )
import Test.Tasty.HUnit
  ( (@?=)
  , testCase
  )

import Q4C12.Defrost
  ( Env
  , env
  , defrost
  , VersionPolicy
  , pvpPolicy
  , pvpLooseImportsPolicy
  , semverPolicy
  , semverLooseImportsPolicy
  , exactPolicy
  )

main :: IO ()
main = defaultMain $ testGroup "defrost tests"
  [ testGroup "should work" successTests
  , testGroup "should fail" failureTests
  ]

successTest :: Map PackageName VersionPolicy -> [ PF.Constraint ] -> SByteString -> [ Env ] -> SByteString -> IO ()
successTest policy extraConstraints input envs expected = do
  let
    inputParseRes = parseGenericPackageDescription input
  case runParseResult inputParseRes of
    (_warns, Left (_versionMay, errs)) ->
      fail $ foldMap (showPError "(input)") errs
    (_warns, Right inputGpd) -> do
      let expectedParseRes = parseGenericPackageDescription expected
      case runParseResult expectedParseRes of
        (_warns, Left (_versionMay, errs)) ->
          fail $ foldMap (showPError "(expected)") errs
        (_warns, Right expectedGpd) -> do
          defrost policy extraConstraints envs inputGpd @?= Right expectedGpd          

successTests :: [TestTree]
successTests =
  [ testCase "simple" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >=3 && <3.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "unqualified only" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.unqualifiedOnly $ thisVersion (mkVersion [3])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >=3 && <3.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "unqualified and setup differ" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "custom-setup"
          , "  setup-depends:"
          , "    foo"
          , "library"
          , "  build-depends:"
          , "    foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.unqualifiedOnly $ thisVersion (mkVersion [3])
                , PF.constraintVersion "foo" PF.qualifiedSetupAll $ thisVersion (mkVersion [4])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "custom-setup"
          , "  setup-depends:"
          , "    foo >= 4 && < 4.1"
          , "library"
          , "  build-depends:"
          , "    foo >=3 && <3.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "unqualified and all" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.unqualifiedOnly $ thisVersion (mkVersion [3])
                ]
          , env System.OpenBSD System.X86_64 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 2]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [5])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >=3 && <3.1 || >=5 && <5.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "leave internal libraries & self-deps alone" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: example-test"
          , "version: 0"
          , ""
          , "library"
          , ""
          , "library some-internal-library"
          , ""
          , "executable foo"
          , "  build-depends:"
          , "    some-internal-library,"
          , "    example-test"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.unqualifiedOnly $ thisVersion (mkVersion [3])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: example-test"
          , "version: 0"
          , ""
          , "library"
          , ""
          , "library some-internal-library"
          , ""
          , "executable foo"
          , "  build-depends:"
          , "    some-internal-library,"
          , "    example-test"
          ]
      successTest mempty [] input freezes expected

  , testCase "setup-depends" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "custom-setup"
          , "  setup-depends:"
          , "    a,"
          , "    b,"
          , "    c"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "a" PF.unqualifiedOnly $ thisVersion (mkVersion [1])
                , PF.constraintVersion "a" PF.qualifiedAll $ thisVersion (mkVersion [2])
                , PF.constraintVersion "b" PF.qualifiedSetupAll $ thisVersion (mkVersion [3])
                , PF.constraintVersion "c" (PF.qualifiedSetup "test") $ thisVersion (mkVersion [4])
                , PF.constraintVersion "c" (PF.qualifiedSetup "other") $ thisVersion (mkVersion [5])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "custom-setup"
          , "  setup-depends:"
          , "    a >=2 && <2.1,"
          , "    b >=3 && < 3.1,"
          , "    c >=4 && <4.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "preconstrained" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo > 1.0.1 || < 1.0.1"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >=1 && <1.0.1 || >1.0.1 && < 1.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "conditional dependency" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if impl(ghc >= 8.4)"
          , "    build-depends:"
          , "      foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if impl(ghc >= 8.4)"
          , "    build-depends:"
          , "      foo >=1 && <1.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "conditional dependency multi-freeze" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if impl(ghc >= 8.4)"
          , "    build-depends:"
          , "      foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          , env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 6]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [2])
                ]
          , env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 2]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if impl(ghc >= 8.4)"
          , "    build-depends:"
          , "      foo >=1 && <1.1 || >= 2 && < 2.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "conditional dependency multi-freeze disjunction" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if impl(ghc >= 8.4) || os(linux)"
          , "    build-depends:"
          , "      foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          , env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 2]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [2])
                ]
          , env System.Windows System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if impl(ghc >= 8.4) || os(linux)"
          , "    build-depends:"
          , "      foo >=1 && <1.1 || >= 2 && < 2.1 || >= 3 && < 3.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "conditional dependency multi-freeze conjunction" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if impl(ghc >= 8.4) && os(linux)"
          , "    build-depends:"
          , "      foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          , env System.Windows System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 6]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [2])
                ]
          , env System.Windows System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 2]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if impl(ghc >= 8.4) && os(linux)"
          , "    build-depends:"
          , "      foo >=1 && <1.1"
          ]
      successTest mempty [] input freezes expected

  , testCase "extra constraints" $ do
      let
        extraConstraints =
          [ PF.constraintVersion "foo" PF.qualifiedAll $ laterVersion (mkVersion [3,0,5])
          ]
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >3.0.5 && <3.1"
          ]
      successTest mempty extraConstraints input freezes expected

  , testCase "versioning policy" $ do
      let
        versioningPolicy :: Map PackageName VersionPolicy
        versioningPolicy = Map.fromList
          [ ( "foo-pvp", pvpPolicy )
          , ( "foo-semver", semverPolicy )
          , ( "foo-pvp-loose-imports", pvpLooseImportsPolicy )
          , ( "foo-semver-loose-imports", semverLooseImportsPolicy )
          , ( "foo-exact", exactPolicy )
          ]
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo-pvp, foo-semver, foo-pvp-loose-imports, foo-semver-loose-imports, foo-exact"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo-pvp" PF.qualifiedAll $ thisVersion (mkVersion [1,2,3,4])
                , PF.constraintVersion "foo-semver" PF.qualifiedAll $ thisVersion (mkVersion [1,2,3,4])
                , PF.constraintVersion "foo-pvp-loose-imports" PF.qualifiedAll $ thisVersion (mkVersion [1,2,3,4])
                , PF.constraintVersion "foo-semver-loose-imports" PF.qualifiedAll $ thisVersion (mkVersion [1,2,3,4])
                , PF.constraintVersion "foo-exact" PF.qualifiedAll $ thisVersion (mkVersion [1,2,3,4])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo-pvp >= 1.2.3.4 && < 1.3,"
          , "    foo-semver >= 1.2.3.4 && < 2,"
          , "    foo-pvp-loose-imports >= 1.2.3.4 && < 1.2.4,"
          , "    foo-semver-loose-imports >= 1.2.3.4 && < 1.3,"
          , "    foo-exact == 1.2.3.4"
          ]
      successTest versioningPolicy [] input freezes expected

  , testCase "versioning policy deep" $ do
      let
        versioningPolicy :: Map PackageName VersionPolicy
        versioningPolicy = Map.fromList
          [ ( "foo", pvpLooseImportsPolicy )
          ]
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >= 1 && < 1.0.1"
          ]
      successTest versioningPolicy [] input freezes expected

  ]

failureTests :: [TestTree]
failureTests =
  [ testCase "missing build-depends" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends: bar, baz"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          ]
        expected = ST.unlines
          [ "Unfrozen build dependency: bar"
          , "Unfrozen build dependency: baz"
          ]
      failureTest input freezes expected

  , testCase "missing setup-depends" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "custom-setup"
          , "  setup-depends: bar, baz"
          ]
        freezes =
          [ env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          ]
        expected = ST.unlines
          [ "Unfrozen setup dependency: bar"
          , "Unfrozen setup dependency: baz"
          ]
      failureTest input freezes expected

  , testCase "conditional dependency only present irrelevantly" $ do
      let
        input = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  if os(linux)"
          , "    build-depends:"
          , "      foo"
          ]
        freezes =
          [ env System.Windows System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              Seq.fromList
                [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                ]
          ]
        expected = ST.unlines
          [ "Unfrozen build dependency: foo"
          ]
      failureTest input freezes expected

  -- TODO: test that an extra constraint without a frozen constraint isn't enough.
  -- TODO: test that a partially frozen constraint isn't enough, even if it's fully tightened by an extra constraint
  ]

failureTest :: SByteString -> [ Env ] -> SText -> IO ()
failureTest input envs expected = do
  let
    inputParseRes = parseGenericPackageDescription input
  case runParseResult inputParseRes of
    (_warns, Left (_versionMay, errs)) ->
      fail $ foldMap (showPError "(input)") errs
    (_warns, Right inputGpd) -> do
      defrost mempty [] envs inputGpd @?= Left expected
