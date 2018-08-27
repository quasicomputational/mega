module Main
  ( main
  )
  where

import Control.Lens
  ( set
  )
import qualified Data.ByteString as BS
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
import Distribution.Parsec.Common
  ( showPError
  )
import qualified Distribution.System as System
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
  )

main :: IO ()
main = defaultMain $ testGroup "defrost tests"
  [ testGroup "should work" successTests
  , testGroup "should fail" failureTests
  ]

successTest :: [ PF.Constraint ] -> SByteString -> [ Env ] -> SByteString -> IO ()
successTest extraConstraints input envs expected = do
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
          defrost extraConstraints envs inputGpd @?= Right expectedGpd          

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                  ]
                )
                PF.emptyConfig
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >=3 && <3.1"
          ]
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.unqualifiedOnly $ thisVersion (mkVersion [3])
                  ]
                )
                PF.emptyConfig
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >=3 && <3.1"
          ]
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.unqualifiedOnly $ thisVersion (mkVersion [3])
                  , PF.constraintVersion "foo" PF.qualifiedSetupAll $ thisVersion (mkVersion [4])
                  ]
                )
                PF.emptyConfig
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
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.unqualifiedOnly $ thisVersion (mkVersion [3])
                  ]
                )
                PF.emptyConfig
          , env System.OpenBSD System.X86_64 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 2]) $
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [5])
                  ]
                )
                PF.emptyConfig
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >=3 && <3.1 || >=5 && <5.1"
          ]
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.unqualifiedOnly $ thisVersion (mkVersion [3])
                  ]
                )
                PF.emptyConfig
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
      successTest [] input freezes expected


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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "a" PF.unqualifiedOnly $ thisVersion (mkVersion [1])
                  , PF.constraintVersion "a" PF.qualifiedAll $ thisVersion (mkVersion [2])
                  , PF.constraintVersion "b" PF.qualifiedSetupAll $ thisVersion (mkVersion [3])
                  , PF.constraintVersion "c" (PF.qualifiedSetup "test") $ thisVersion (mkVersion [4])
                  , PF.constraintVersion "c" (PF.qualifiedSetup "other") $ thisVersion (mkVersion [5])
                  ]
                )
                PF.emptyConfig
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
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                  ]
                )
                PF.emptyConfig
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >=1 && <1.0.1 || >1.0.1 && < 1.1"
          ]
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                  ]
                )
                PF.emptyConfig
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
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                  ]
                )
                PF.emptyConfig
          , env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 6]) $
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [2])
                  ]
                )
                PF.emptyConfig
          , env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 2]) $
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                  ]
                )
                PF.emptyConfig
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
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                  ]
                )
                PF.emptyConfig
          , env System.Linux System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 2]) $
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [2])
                  ]
                )
                PF.emptyConfig
          , env System.Windows System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 4]) $
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                  ]
                )
                PF.emptyConfig
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
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                  ]
                )
                PF.emptyConfig
          , env System.Windows System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 6]) $
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [2])
                  ]
                )
                PF.emptyConfig
          , env System.Windows System.I386 (mkFlagAssignment []) Compiler.GHC (mkVersion [8, 2]) $
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                  ]
                )
                PF.emptyConfig
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
      successTest [] input freezes expected

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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [3])
                  ]
                )
                PF.emptyConfig
          ]
        expected = BS.intercalate "\n"
          [ "cabal-version: 2.2"
          , "name: test"
          , "version: 0"
          , "library"
          , "  build-depends:"
          , "    foo >3.0.5 && <3.1"
          ]
      successTest extraConstraints input freezes expected
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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                  ]
                )
                PF.emptyConfig
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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                  ]
                )
                PF.emptyConfig
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
              set PF.constraints
                ( Seq.fromList
                  [ PF.constraintVersion "foo" PF.qualifiedAll $ thisVersion (mkVersion [1])
                  ]
                )
                PF.emptyConfig
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
      defrost [] envs inputGpd @?= Left expected
