module Main
  ( main
  )
  where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTEnc
import qualified Data.Text.Lazy.Builder as LTB
import Test.Tasty (TestTree, TestName, testGroup, defaultMain)
import Test.Tasty.ExpectedFailure (expectFail, ignoreTest)
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import Q4C12.XML (parseXML, renderXML, DoctypeResolver, systemResolver, noEntities)
import Q4C12.XML.Desc.Parse (parse)
import Q4C12.XML.Entity (htmlResolver, entitySetSchema, generateDTD)
import Q4C12.XML.XHTML2HTML (htmlDocument, displayHTMLExceptionPos)
import qualified Q4C12.XML as XML

--TODO: Desc tests?

main :: IO ()
main = do
  xmlTests <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/parse-render"
  xmlTestsWithStdErr <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/parse-render"
  xmlFailTests <- fmap dropExtension <$>
    findByExtension [".fails"] "test/golden/parse-render"
  htmlTests <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/html-entities"
  htmlTestsWithStdErr <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/html-entities"
  htmlFailTests <- fmap dropExtension <$>
    findByExtension [".fails"] "test/golden/html-entities"
  dtdGenerationTests <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/dtd-generation"
  x2hTests <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/xhtml2html"
  x2hFailTests <- fmap dropExtension <$>
    findByExtension [".fails"] "test/golden/xhtml2html"
  defaultMain $ testGroup "xml parsing and re-rendering"
    [ testGroup "should work" $ testXML exampleResolver <$> xmlTests
    , testGroup "should work with stderr" $ testXMLStderr exampleResolver <$> xmlTestsWithStdErr
    , testGroup "should fail" $ testXMLShouldFail exampleResolver <$> xmlFailTests
    , testGroup "with HTML entities"
      [ testGroup "should work" $ testXML htmlResolver <$> htmlTests
      , testGroup "should work with stderr" $ testXMLStderr htmlResolver <$> htmlTestsWithStdErr
      , testGroup "should fail" $ testXMLShouldFail htmlResolver <$> htmlFailTests
      ]
    , testGroup "DTD generation" $ testDTDGeneration <$> dtdGenerationTests
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

exampleResolver :: DoctypeResolver
exampleResolver = systemResolver $ Map.singleton "example:entity-set" $ \case
  "copy" -> Just "\xA9"
  "NewLine" -> Just "\xA"
  _ -> Nothing

testXML :: DoctypeResolver -> TestName -> TestTree
testXML resolver baseName = checkExpectancy $
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName ".out") $ do
    res <- parseXML resolver <$> STIO.readFile (addExtension baseName "in")
    case res of
      (Left err, _) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err
      (Right el, _) ->
        --NB: don't check for no warnings, because we have separate .errout tests here.
        pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ renderXML el
  where
    checkExpectancy = if Set.member baseName expectXMLBroken
      then expectFail
      else id

testXMLShouldFail :: DoctypeResolver -> TestName -> TestTree
testXMLShouldFail resolver baseName = checkExpectancy $
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName ".out") $ do
    res <- parseXML resolver <$> STIO.readFile (addExtension baseName "fails")
    case res of
      (Left err, warns) -> pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ XML.displayWarnings warns <> XML.displayError err <> "\n"
      (Right el, _) -> fail $ "Parsing succeeded: " <> show el
  where
    checkExpectancy = if Set.member baseName expectXMLShouldFailBroken
      then expectFail
      else id

testXMLStderr :: DoctypeResolver -> TestName -> TestTree
testXMLStderr resolver baseName = checkExpectancy $
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName "errout") $ do
    xmlMay <- parseXML resolver <$> STIO.readFile (addExtension baseName "in")
    case xmlMay of
      (Left err, warns) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err <> "\n" <> XML.displayWarnings warns
      (Right _, warns) -> pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ XML.displayWarnings warns
  where
    -- If the primary test's broken, this one will be worthless, so out-and-out ignore it.
    checkExpectancy = if Set.member baseName expectXMLBroken
      then ignoreTest
      else id

testDTDGeneration :: TestName -> TestTree
testDTDGeneration baseName =
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName "out") $ do
    xmlMay <- parseXML noEntities <$> STIO.readFile (addExtension baseName "in")
    case xmlMay of
      (Left err, warns) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err <> "\n" <> XML.displayWarnings warns
      (Right xml, warns) -> do
        unless (null warns) $
          fail $ LT.unpack $ LTB.toLazyText $ XML.displayWarnings warns
        case parse entitySetSchema xml of
          Nothing -> fail "Could not parse the entity set."
          Just entities ->
            pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ generateDTD entities

testXHTML2HTML :: TestName -> TestTree
testXHTML2HTML baseName =
  goldenVsStringDiff baseName (\ref new -> ["diff", "-u", ref, new]) (addExtension baseName "out") $ do
    xmlMay <- parseXML htmlResolver <$> STIO.readFile (addExtension baseName "in")
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
    xmlMay <- parseXML htmlResolver <$> STIO.readFile (addExtension baseName "fails")
    case xmlMay of
      (Left err, warns) -> fail $ LT.unpack $ LTB.toLazyText $ XML.displayError err <> "\n" <> XML.displayWarnings warns
      (Right xml, warns) -> do
        unless (null warns) $
          fail $ LT.unpack $ LTB.toLazyText $ XML.displayWarnings warns
        case htmlDocument xml of
          Left err -> pure $ LTEnc.encodeUtf8 $ LTB.toLazyText $ displayHTMLExceptionPos err
          Right html -> fail "Unexpected success!"
