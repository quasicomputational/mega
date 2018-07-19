module Main
  ( main
  )
  where

import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO

import Q4C12.XML (parseXML)
import qualified Q4C12.XML as XML
import Q4C12.XHTML2HTML (htmlDocument, displayHTMLExceptionPos)

main :: IO ()
main = do
  (res, warns) <- parseXML <$> STIO.getContents
  LTIO.hPutStr stderr $ LTB.toLazyText $ XML.displayWarnings warns
  case res of
    Left err -> do
      LTIO.hPutStrLn stderr $ LTB.toLazyText $ XML.displayError err
      exitFailure
    Right xhtml -> case htmlDocument xhtml of
      Left err -> do
        LTIO.hPutStrLn stderr $ LTB.toLazyText $ displayHTMLExceptionPos err
        exitFailure
      Right output -> LTIO.putStr $ LTB.toLazyText output
