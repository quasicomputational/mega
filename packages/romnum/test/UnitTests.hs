module Main
  ( main
  )
  where

import qualified Data.Text.Lazy.Builder as TBuilder
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Q4C12.RomNum
  (buildRomNum, asciiUppercaseRomanNumeralStyle, fancyUppercaseRomanNumeralStyle)

specs :: [(Natural, LText, LText)]
specs =
  [ (1, "I", "\x2160")
  , (2, "II", "\x2160\x2160")
  , (4, "IV", "\x2160\x2164")
  , (5, "V", "\x2164")
  , (8, "VIII", "\x2164\x2160\x2160\x2160")
  , (9, "IX", "\x2160\x2169")
  , (10, "X", "\x2169")
  , (21, "XXI", "\x2169\x2169\x2160")
  , (27, "XXVII", "\x2169\x2169\x2164\x2160\x2160")
  , (29, "XXIX", "\x2169\x2169\x2160\x2169")
  , (40, "XL", "\x2169\x216C")
  , (43, "XLIII", "\x2169\x216C\x2160\x2160\x2160")
  , (48, "XLVIII", "\x2169\x216C\x2164\x2160\x2160\x2160")
  , (49, "IL", "\x2160\x216C")
  , (99, "IC", "\x2160\x216D")
  , (123, "CXXIII", "\x216D\x2169\x2169\x2160\x2160\x2160")
  , (420, "CDXX", "\x216D\x216E\x2169\x2169")
  , (488, "CDLXXXVIII", "\x216D\x216E\x216C\x2169\x2169\x2169\x2164\x2160\x2160\x2160")
  , (492, "XDII", "\x2169\x216E\x2160\x2160")
  , (499, "ID", "\x2160\x216E")
  , (890, "DCCCXC", "\x216E\x216D\x216D\x216D\x2169\x216D")
  , (901, "CMI", "\x216D\x216F\x2160")
  , (972, "CMLXXII", "\x216D\x216F\x216C\x2169\x2169\x2160\x2160")
  , (994, "XMIV", "\x2169\x216F\x2160\x2164")
  , (999, "IM", "\x2160\x216F")
  , (1450, "MCDL", "\x216F\x216D\x216E\x216C")
  , (2017, "MMXVII", "\x216F\x216F\x2169\x2164\x2160\x2160")
  ]


main :: IO ()
main = defaultMain $ testGroup "roman numeral tests" $
  flip foldMap specs $ \ (n, plain, fancy) ->
    [ testCase ("plain conversion of " <> show n) $
        TBuilder.toLazyText (buildRomNum asciiUppercaseRomanNumeralStyle (n - 1)) @?= plain
    , testCase ("fancy conversion of " <> show n) $
        TBuilder.toLazyText (buildRomNum fancyUppercaseRomanNumeralStyle (n - 1)) @?= fancy
    ]
