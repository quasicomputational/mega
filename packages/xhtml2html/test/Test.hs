module Main
  ( main
  )
  where

import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTEnc
import qualified Data.Text.Lazy.Builder as LTB
import Q4C12.XML (parseXML)
import qualified Q4C12.XML as XML
import Test.Tasty (TestTree, TestName, testGroup, defaultMain)
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import Q4C12.XHTML2HTML (htmlDocument, displayHTMLExceptionPos)

main :: IO ()
main = do
  x2hTests <- fmap dropExtension <$>
    findByExtension [".in"] "test/golden/xhtml2html"
  x2hFailTests <- fmap dropExtension <$>
    findByExtension [".fails"] "test/golden/xhtml2html"
  defaultMain $ testGroup "xhtml2html"
    [ testGroup "should work" $ testXHTML2HTML <$> x2hTests
    , testGroup "should fail" $ testXHTML2HTMLShouldFail <$> x2hFailTests
    ]

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
