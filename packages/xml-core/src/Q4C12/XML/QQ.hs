module Q4C12.XML.QQ
  ( xmlqq
  )
  where

import Data.String
  ( String
  )
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Language.Haskell.TH
  ( Exp
  , Q
  )
import Language.Haskell.TH.Quote
  ( QuasiQuoter ( QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec )
  )
import Language.Haskell.TH.Syntax
  ( liftData
  )
import Q4C12.Position
  ( PositionRange
  )

import Q4C12.XML.Internal
  ( Element
  , displayError
  , parseXML
  )

xmlqq :: QuasiQuoter
xmlqq = QuasiQuoter
  { quoteExp = xmlQuoteExp
  , quotePat = \ _ ->
      fail "xmlqq only works in an expression context"
  , quoteType = \ _ ->
      fail "xmlqq only works in an expression context"
  , quoteDec = \ _ ->
      fail "xmlqq only works in an expression context"
  }

xmlQuoteExp :: String -> Q Exp
xmlQuoteExp input = case parseXML $ ST.pack input of
  ( Left err, _warns ) ->
    fail $ LT.unpack $ LTB.toLazyText $ displayError err
  ( Right el, _warns ) ->
    liftData (el :: Element Void PositionRange)
