module Main
  ( main
  )
  where

import qualified Data.Set as Set
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTEnc
import qualified Data.Text.Lazy.Builder as LTB
import Test.Tasty (TestTree, TestName, testGroup, defaultMain)
import Test.Tasty.ExpectedFailure (expectFail, ignoreTest)
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import Q4C12.XML (parseXML, parseXML', parseXMLCommented, preRootComments, rootElement, postRootComments, renderXML, uname, element, markupElement, markupText, Comment (Comment))
import Q4C12.XML.XHTML2HTML (htmlDocument, displayHTMLExceptionPos)
import qualified Q4C12.XML as XML

--TODO: Desc tests?

main :: IO ()
main = do
  xmlTests <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/parse-render"
  preserveCommentTests <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/comments"
  xmlTestsWithStdErr <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/parse-render"
  xmlFailTests <- fmap dropExtension <$>
    findByExtension [".fails"] "test/golden/parse-render"
  x2hTests <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/xhtml2html"
  x2hFailTests <- fmap dropExtension <$>
    findByExtension [".fails"] "test/golden/xhtml2html"
  defaultMain $ testGroup "xml parsing and re-rendering"
    [ testGroup "should work" $ testXML <$> xmlTests
    , testGroup "comment preserving" $ testXMLPreserveComments <$> preserveCommentTests
    , testGroup "should work with stderr" $ testXMLStderr <$> xmlTestsWithStdErr
    , testGroup "should fail" $ testXMLShouldFail <$> xmlFailTests
    , testGroup "xhtml2html" $
        [ testGroup "should work" $ testXHTML2HTML <$> x2hTests
        , testGroup "should fail" $ testXHTML2HTMLShouldFail <$> x2hFailTests
        ]
    ]

expectXMLBroken :: Set TestName
expectXMLBroken = Set.fromList
  [
  ]

expectXMLShouldFailBroken :: Set TestName
expectXMLShouldFailBroken = Set.fromList
  [
  ]

testXML :: TestName -> TestTree
testXML baseName = checkExpectancy $
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName ".out") $ do
    res <- parseXML <$> STIO.readFile (addExtension baseName "in")
    case res of
      (Left err, _) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err
      (Right el, _) ->
        --NB: don't check for no warnings, because we have separate .errout tests here.
        pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ renderXML el
  where
    checkExpectancy = if Set.member baseName expectXMLBroken
      then expectFail
      else id

testXMLPreserveComments :: TestName -> TestTree
testXMLPreserveComments baseName = checkExpectancy $
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName ".out") $ do
    resOrErr <- parseXMLCommented <$> STIO.readFile (addExtension baseName "in")
    case resOrErr of
      (Left err, _) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err
      (Right res, _) -> do
        let wrapComment :: Comment pos -> XML.Markup cmt ()
            wrapComment (Comment tree) = markupElement $
              element (uname "comment") $ markupText $ foldMap snd tree
            el = element (uname "result") $ foldMap markupElement
              [ element (uname "pre-root") $
                  foldMapOf (preRootComments . traverse) wrapComment res
              , () <$ view rootElement res
              , element (uname "post-root") $
                  foldMapOf (postRootComments . traverse) wrapComment res
              ]
        pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ renderXML el
  where
    checkExpectancy = if Set.member baseName expectXMLBroken
      then expectFail
      else id

testXMLShouldFail :: TestName -> TestTree
testXMLShouldFail baseName = checkExpectancy $
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName ".out") $ do
    res <- parseXML' <$> STIO.readFile (addExtension baseName "fails")
    case res of
      (Left err, warns) -> pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ XML.displayWarnings warns <> XML.displayError err <> "\n"
      (Right el, _) -> fail $ "Parsing succeeded: " <> show el
  where
    checkExpectancy = if Set.member baseName expectXMLShouldFailBroken
      then expectFail
      else id

testXMLStderr :: TestName -> TestTree
testXMLStderr baseName = checkExpectancy $
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName "errout") $ do
    xmlMay <- parseXML <$> STIO.readFile (addExtension baseName "in")
    case xmlMay of
      (Left err, warns) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err <> "\n" <> XML.displayWarnings warns
      (Right _, warns) -> pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ XML.displayWarnings warns
  where
    -- If the primary test's broken, this one will be worthless, so out-and-out ignore it.
    checkExpectancy = if Set.member baseName expectXMLBroken
      then ignoreTest
      else id

testXHTML2HTML :: TestName -> TestTree
testXHTML2HTML baseName =
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName "out") $ do
    xmlMay <- parseXML <$> STIO.readFile (addExtension baseName "in")
    case xmlMay of
      (Left err, warns) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err <> "\n" <> XML.displayWarnings warns
      (Right xml, warns) -> do
        unless (null warns) $
          fail $ LT.unpack $ LTB.toLazyText $ XML.displayWarnings warns
        case htmlDocument xml of
          Left err -> fail $ "XHTML2HTML failed: " <> LT.unpack (LTB.toLazyText $ displayHTMLExceptionPos err)
          Right html ->
            pure $ LTEnc.encodeUtf8 $ LTB.toLazyText html

testXHTML2HTMLShouldFail :: TestName -> TestTree
testXHTML2HTMLShouldFail baseName =
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName "out") $ do
    xmlMay <- parseXML <$> STIO.readFile (addExtension baseName "fails")
    case xmlMay of
      (Left err, warns) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err <> "\n" <> XML.displayWarnings warns
      (Right xml, warns) -> do
        unless (null warns) $
          fail $ LT.unpack $ LTB.toLazyText $ XML.displayWarnings warns
        case htmlDocument xml of
          Left err -> pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ displayHTMLExceptionPos err
          Right _ -> fail "Unexpected success!"
