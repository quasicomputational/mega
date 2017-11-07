module Q4C12.RomNum
  ( RomanNumeralStyle
  , fancyUppercaseRomanNumeralStyle, asciiUppercaseRomanNumeralStyle
  , buildRomNum
  )
  where

data RomanNumeralStyle = RomanNumeralStyle
  { romNumI :: TBuilder
  , romNumV :: TBuilder
  , romNumX :: TBuilder
  , romNumL :: TBuilder
  , romNumC :: TBuilder
  , romNumD :: TBuilder
  , romNumM :: TBuilder
  }

fancyUppercaseRomanNumeralStyle :: RomanNumeralStyle
fancyUppercaseRomanNumeralStyle = RomanNumeralStyle
  "\x2160" "\x2164" "\x2169" "\x216C" "\x216D" "\x216E" "\x216F"

asciiUppercaseRomanNumeralStyle :: RomanNumeralStyle
asciiUppercaseRomanNumeralStyle = RomanNumeralStyle
  "I" "V" "X" "L" "C" "D" "M"

--TODO: extend to the really big numerals?
buildRomNum :: RomanNumeralStyle -> Natural -> TBuilder
buildRomNum style = go . (+ 1)
  where
    go :: Natural -> TBuilder
    go n
      | n >= 1000 = fold
        [ romNumM style,  go (n - 1000) ]
      | n >= 999 = fold
        [ romNumI style, romNumM style, go (n - 999) ]
      | n >= 990 = fold
        [ romNumX style, romNumM style, go (n - 990) ]
      | n >= 900 = fold
        [ romNumC style, romNumM style, go (n - 900) ]
      | n >= 500 = fold
        [ romNumD style, go (n - 500) ]
      | n >= 499 = fold
        [ romNumI style, romNumD style, go (n - 499) ]
      | n >= 490 = fold
        [ romNumX style, romNumD style, go (n - 490) ]
      | n >= 400 = fold
        [ romNumC style, romNumD style, go (n - 400) ]
      | n >= 100 = fold
        [ romNumC style, go (n - 100) ]
      | n >= 99 = fold
        [ romNumI style, romNumC style, go (n - 99) ]
      | n >= 90 = fold
        [ romNumX style, romNumC style, go (n - 90) ]
      | n >= 50 = fold
        [ romNumL style, go (n - 50) ]
      | n >= 49 = fold
        [ romNumI style, romNumL style, go (n - 49) ]
      | n >= 40 = fold
        [ romNumX style, romNumL style, go (n - 40) ]
      | n >= 10 = fold
        [ romNumX style, go (n - 10) ]
      | n >= 9 = fold
        [ romNumI style, romNumX style, go (n - 9) ]
      | n >= 5 = fold
        [ romNumV style, go (n - 5) ]
      | n >= 4 = fold
        [ romNumI style, romNumV style, go (n - 4) ]
      | otherwise = mtimesSafe n $ romNumI style
