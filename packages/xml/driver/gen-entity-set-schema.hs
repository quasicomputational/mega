module Main (main) where

import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO

import Q4C12.XML (renderXML)
import Q4C12.XML.Desc.RELAX (relax)
import Q4C12.XML.TH (entitySetSchema)

main :: IO ()
main = LTIO.putStr $ LTB.toLazyText $ renderXML $ relax entitySetSchema
