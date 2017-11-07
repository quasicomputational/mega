module Main
  ( main
  )
  where

import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO

import Q4C12.XML (parseXML, noEntities, displayError, displayWarnings)
import Q4C12.XML.Desc.Parse (parse)
import Q4C12.XML.Entity (entitySetSchema, generateDTD)

main :: IO ()
main = do
  (xmlMay, warns) <- parseXML noEntities <$> STIO.getContents
  LTIO.hPutStr stderr $ LTB.toLazyText $ displayWarnings warns
  case xmlMay of
    Left err -> do
      LTIO.hPutStr stderr $ LTB.toLazyText $ displayError err <> "\n"
      exitFailure
    Right xml -> case parse entitySetSchema xml of
      Nothing -> do
        LTIO.hPutStrLn stderr "Could not parse the entity set."
        exitFailure
      Just entities ->
        LTIO.putStr $ LTB.toLazyText $ generateDTD entities
