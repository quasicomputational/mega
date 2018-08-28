module Main
  ( main
  )
  where

import qualified Data.Sequence as Seq
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Distribution.Types.Version
  ( mkVersion
  )
import Distribution.Types.VersionRange
  ( anyVersion
  , earlierVersion
  , intersectVersionRanges
  , laterVersion
  , majorBoundVersion
  , noVersion
  , orEarlierVersion
  , orLaterVersion
  , thisVersion
  , unionVersionRanges
  )
import Test.Tasty
  ( TestName
  , TestTree
  , defaultMain
  , testGroup
  )
import Test.Tasty.HUnit
  ( Assertion
  , (@?=)
  , testCase
  )

import qualified Q4C12.ProjectFile as PF

checkPackages :: SText -> [ SText ] -> Assertion
checkPackages input expected =
  case PF.parse input of
    Left err ->
      fail $ ST.unpack $ PF.renderError err
    Right config ->
      Seq.sort ( view PF.packages config ) @?= Seq.sort ( Seq.fromList expected )

packagesOneLiner :: TestTree
packagesOneLiner = testCase "one-liner" $ checkPackages input output
  where
    input = ST.unlines
      [ "packages: a b ./c/d e/f"
      ]
    output :: [ SText ]
    output =
      [ "a"
      , "b"
      , "./c/d"
      , "e/f"
      ]

packagesLeadingComment :: TestTree
packagesLeadingComment = testCase "leading comment" $ checkPackages input output
  where
    input = ST.unlines
      [ "-- Hello!"
      , "packages: a"
      ]
    output :: [ SText ]
    output =
      [ "a"
      ]

packagesCommentedOut :: TestTree
packagesCommentedOut = testCase "commented out" $ checkPackages input output
  where
    input = ST.unlines
      [ "packages:"
      , "  foo"
      , "  -- bar"
      , "  baz"
      ]
    output :: [ SText ]
    output =
      [ "foo"
      , "baz"
      ]

packagesSameStartLine :: TestTree
packagesSameStartLine = testCase "same start line" $ checkPackages input output
  where
    input = ST.unlines
      [ "packages: one"
      , "          two"
      ]
    output :: [ SText ]
    output =
      [ "one"
      , "two"
      ]

packagesDifferentStartLine :: TestTree
packagesDifferentStartLine = testCase "different start line" $ checkPackages input output
  where
    input = ST.unlines
      [ "packages:"
      , "  one"
      , "  two"
      ]
    output :: [ SText ]
    output =
      [ "one"
      , "two"
      ]

packagesUnindentedComment :: TestTree
packagesUnindentedComment = testCase "unindented comment" $ checkPackages input output
  where
    input = ST.unlines
      [ "packages:"
      , "  foo"
      , "-- bar"
      , "  baz"
      ]
    output :: [ SText ]
    output =
      [ "foo"
      , "baz"
      ]

packagesBlankLine :: TestTree
packagesBlankLine = testCase "blank line" $ checkPackages input output
  where
    input = ST.unlines
      [ "packages:"
      , "  foo"
      , ""
      , "  bar"
      ]
    output :: [ SText ]
    output =
      [ "foo"
      , "bar"
      ]

packagesUnknownFieldFollowing :: TestTree
packagesUnknownFieldFollowing = testCase "unknown field following" $ checkPackages input output
  where
    input = ST.unlines
      [ "packages:"
      , "  foo"
      , "flargle: spam"
      ]
    output :: [ SText ]
    output =
      [ "foo"
      ]

packagesMulti :: TestTree
packagesMulti = testCase "multi" $ checkPackages input output
  where
    input = ST.unlines
      [ "packages:"
      , "  foo"
      , "flargle: spam"
      , "packages:"
      , "  bar"
      ]
    output :: [ SText ]
    output =
      [ "foo"
      , "bar"
      ]

packagesNone :: TestTree
packagesNone = testCase "none" $ checkPackages input output
  where
    input = ST.unlines
      [ "flargle: spam"
      ]
    output :: [ SText ]
    output =
      [
      ]

packagesAndSections :: TestTree
packagesAndSections = testCase "and sections" $ checkPackages input output
  where
    input = ST.unlines
      [ "some-section-stanza foo"
      , "  hello: bar"
      , ""
      , "  packages: nope"
      , "packages: a b c"
      , "some-other-section"
      , "  blah: blah"
      ]
    output :: [ SText ]
    output =
      [ "a"
      , "b"
      , "c"
      ]

packageParseTests :: TestTree
packageParseTests = testGroup "packages"
  [ packagesOneLiner
  , packagesLeadingComment
  , packagesCommentedOut
  , packagesSameStartLine
  , packagesDifferentStartLine
  , packagesUnindentedComment
  , packagesBlankLine
  , packagesUnknownFieldFollowing
  , packagesMulti
  , packagesNone
  , packagesAndSections
  ]

checkConstraints :: SText -> [ PF.Constraint ] -> Assertion
checkConstraints input expected =
  case PF.parse input of
    Left err ->
      fail $ ST.unpack $ PF.renderError err
    Right config ->
      Seq.sort ( view PF.constraints config ) @?= Seq.sort ( Seq.fromList expected )

constraintsSameStartLine :: TestTree
constraintsSameStartLine = testCase "same start line" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints: bar == 2.1,"
      , "             baz > 3"
      ]
    output =
      [ PF.constraintVersion "bar" PF.unqualifiedOnly ( thisVersion $ mkVersion [ 2, 1 ] )
      , PF.constraintVersion "baz" PF.unqualifiedOnly ( laterVersion $ mkVersion [ 3 ] )
      ]

constraintsFlags :: TestTree
constraintsFlags = testCase "flags" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints:"
      , "  spam -eggs +ham +beans"
      ]
    output =
      [ PF.constraintFlag "spam" PF.unqualifiedOnly False "eggs"
      , PF.constraintFlag "spam" PF.unqualifiedOnly True "ham"
      , PF.constraintFlag "spam" PF.unqualifiedOnly True "beans"
      ]

constraintsComplicatedExpressions :: TestTree
constraintsComplicatedExpressions = testCase "complicated expressions" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints:"
      , "  foo >= 2 && < 4 || >= 4.1 && < 4.3,"
      , "  bar <= 1 || > 2,"
      , "  baz ^>= 2.3.45"
      ]
    output =
      [ PF.constraintVersion "foo" PF.unqualifiedOnly
          ( unionVersionRanges
            ( intersectVersionRanges
              ( orLaterVersion ( mkVersion [ 2 ] ) )
              ( earlierVersion ( mkVersion [ 4 ] ) ) )
            ( intersectVersionRanges
              ( orLaterVersion ( mkVersion [ 4, 1 ] ) )
              ( earlierVersion ( mkVersion [ 4, 3 ] ) ) ) )
      , PF.constraintVersion "bar" PF.unqualifiedOnly
          ( unionVersionRanges
            ( orEarlierVersion ( mkVersion [ 1 ] ) )
            ( laterVersion ( mkVersion [ 2 ] ) ) )
      , PF.constraintVersion "baz" PF.unqualifiedOnly
          ( majorBoundVersion ( mkVersion [ 2, 3, 45 ] ) )
      ]

constraintsAnyNone :: TestTree
constraintsAnyNone = testCase "any, none" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints:"
      , "  foo -any,"
      , "  bar -none"
      ]
    output =
      [ PF.constraintVersion "foo" PF.unqualifiedOnly anyVersion
      , PF.constraintVersion "bar" PF.unqualifiedOnly noVersion
      ]

constraintsSourceInstalled :: TestTree
constraintsSourceInstalled = testCase "source, installed" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints:"
      , "  foo source,"
      , "  bar installed"
      ]
    output =
      [ PF.constraintSource "foo" PF.unqualifiedOnly
      , PF.constraintInstalled "bar" PF.unqualifiedOnly
      ]

constraintsTestBench :: TestTree
constraintsTestBench = testCase "test, bench" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints:"
      , "  foo test,"
      , "  foo bench"
      ]
    output =
      [ PF.constraintTest "foo" PF.unqualifiedOnly
      , PF.constraintBench "foo" PF.unqualifiedOnly
      ]

constraintsMulti :: TestTree
constraintsMulti = testCase "multi" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints: foobar == 1.0"
      , "constraints: spam == 2.3"
      ]
    output =
      [ PF.constraintVersion "foobar" PF.unqualifiedOnly ( thisVersion ( mkVersion [ 1, 0 ] ) )
      , PF.constraintVersion "spam" PF.unqualifiedOnly ( thisVersion ( mkVersion [ 2, 3 ] ) )
      ]

-- TODO: build-tool-depends qualification; the Cabal docs don't have an example of the syntax
constraintsQualified :: TestTree
constraintsQualified = testCase "qualification" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints:"
      , "  any.foo == 3,"
      , "  setup.bar == 7,"
      , "  blah:setup.baz == 9"
      ]
    output =
      [ PF.constraintVersion "foo" PF.qualifiedAll ( thisVersion ( mkVersion [ 3 ] ) )
      , PF.constraintVersion "bar" PF.qualifiedSetupAll ( thisVersion ( mkVersion [ 7 ] ) )
      , PF.constraintVersion "baz" ( PF.qualifiedSetup "blah" ) ( thisVersion ( mkVersion [ 9 ] ) )
      ]

constraintsLeadingCommas :: TestTree
constraintsLeadingCommas = testCase "leading commas" $ checkConstraints input output
  where
    input = ST.unlines
      [ "constraints:"
      , "    foo test"
      , "  , bar bench"
      ]
    output =
      [ PF.constraintTest "foo" PF.unqualifiedOnly
      , PF.constraintBench "bar" PF.unqualifiedOnly
      ]

constraintsNone :: TestTree
constraintsNone = testCase "no constraints" $ checkConstraints input output
  where
    input :: SText
    input = "--Hello!\n"
    output :: [PF.Constraint]
    output = []

constraintParseTests :: TestTree
constraintParseTests = testGroup "constraints"
  [ constraintsSameStartLine
  , constraintsFlags
  , constraintsComplicatedExpressions
  , constraintsAnyNone
  , constraintsSourceInstalled
  , constraintsTestBench
  , constraintsMulti
  , constraintsQualified
  , constraintsLeadingCommas
  , constraintsNone
  ]

constraintRenderTest :: TestName -> SText -> PF.Constraint -> TestTree
constraintRenderTest testName output cstr = testCase testName $
  LT.toStrict ( LTB.toLazyText ( PF.renderConstraint cstr ) ) @?= output

constraintRenderTests :: TestTree
constraintRenderTests = testGroup "constraints"
  [ constraintRenderTest "thisVersion" "foo == 3.4.2" $
      PF.constraintVersion "foo" PF.unqualifiedOnly ( thisVersion $ mkVersion [ 3, 4, 2 ] )
  , constraintRenderTest "earlierVersion" "foo < 4" $
      PF.constraintVersion "foo" PF.unqualifiedOnly ( earlierVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "orEarlierVersion" "foo <= 4" $
      PF.constraintVersion "foo" PF.unqualifiedOnly ( orEarlierVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "laterVersion" "foo > 4" $
      PF.constraintVersion "foo" PF.unqualifiedOnly ( laterVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "orLaterVersion" "foo >= 4" $
      PF.constraintVersion "foo" PF.unqualifiedOnly ( orLaterVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "intersection" "foo > 3 && < 4" $
      PF.constraintVersion "foo" PF.unqualifiedOnly $
        intersectVersionRanges
          ( laterVersion $ mkVersion [ 3 ] )
          ( earlierVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "union" "foo < 3 || > 4" $
      PF.constraintVersion "foo" PF.unqualifiedOnly $
        unionVersionRanges
          ( earlierVersion $ mkVersion [ 3 ] )
          ( laterVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "complicated" "foo >= 3 && < 3.5 || >= 4 && < 4.1" $
      PF.constraintVersion "foo" PF.unqualifiedOnly $
        unionVersionRanges
          ( intersectVersionRanges
              ( orLaterVersion $ mkVersion [ 3 ] )
              ( earlierVersion $ mkVersion [ 3, 5 ] )
          )
          ( intersectVersionRanges
              ( orLaterVersion $ mkVersion [ 4 ] )
              ( earlierVersion $ mkVersion [ 4, 1 ] )
          )
  , constraintRenderTest "impossible" "foo -none" $
      PF.constraintVersion "foo" PF.unqualifiedOnly $
        intersectVersionRanges
          ( thisVersion $ mkVersion [ 1 ] )
          ( thisVersion $ mkVersion [ 2 ] )
  , constraintRenderTest "all qualification" "any.foo >= 4" $
      PF.constraintVersion "foo" PF.qualifiedAll ( orLaterVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "all setup qualification" "setup.foo >= 4" $
      PF.constraintVersion "foo" PF.qualifiedSetupAll ( orLaterVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "specific setup qualification" "bar:setup.foo >= 4" $
      PF.constraintVersion "foo" ( PF.qualifiedSetup "bar" ) ( orLaterVersion $ mkVersion [ 4 ] )
  , constraintRenderTest "test constraint" "foo test" $
      PF.constraintTest "foo" PF.unqualifiedOnly
  , constraintRenderTest "bench constraint" "foo bench" $
      PF.constraintBench "foo" PF.unqualifiedOnly
  , constraintRenderTest "source constraint" "foo source" $
      PF.constraintSource "foo" PF.unqualifiedOnly
  , constraintRenderTest "installed constraint" "foo installed" $
      PF.constraintInstalled "foo" PF.unqualifiedOnly
  , constraintRenderTest "false flag" "foo -bar" $
      PF.constraintFlag "foo" PF.unqualifiedOnly False "bar"
  , constraintRenderTest "true flag" "foo +bar" $
      PF.constraintFlag "foo" PF.unqualifiedOnly True "bar"
  ]

main :: IO ()
main = defaultMain $ testGroup "project-file tests"
  [ testGroup "parsing"
    [ packageParseTests
    , constraintParseTests
    ]
  , testGroup "rendering"
    [ constraintRenderTests
    ]
  ]
