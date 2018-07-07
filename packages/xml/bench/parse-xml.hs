module Main
  ( main
  )
  where

import Criterion.Main (defaultMain, bench, nf)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

import Q4C12.XML (parseXML, noEntities)

input :: Natural -> SText
input n = LT.toStrict $ LTB.toLazyText $ "<foo>" <> mtimesSafe n "<spam>Hello! This gets repeated a lot!</spam>" <> "</foo>"

main :: IO ()
main = defaultMain
  [ bench "Parse 10000" $ nf (parseXML noEntities) (input 10000)
  , bench "Parse 5000" $ nf (parseXML noEntities) (input 5000)
  , bench "Parse 1000" $ nf (parseXML noEntities) (input 1000)
  , bench "Parse 500" $ nf (parseXML noEntities) (input 500)
  ]
