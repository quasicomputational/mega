module Q4C12.AsParsedText
  ( AsParsedText
    ( AsParsedText
    , fromAsParsedText
    )
  )
  where

import qualified Data.Aeson as Aeson
import qualified Data.Text as ST
import Distribution.Parsec
  ( Parsec
  , eitherParsec
  )
import Distribution.Pretty
  ( Pretty
  , prettyShow
  )

newtype AsParsedText a = AsParsedText { fromAsParsedText :: a }
  deriving stock ( Eq, Ord, Show )

instance ( Parsec a ) => Aeson.FromJSON ( AsParsedText a ) where
  parseJSON = Aeson.withText "Cabal value" $
    either fail ( pure . AsParsedText ) . eitherParsec . ST.unpack

instance ( Pretty a ) => Aeson.ToJSON ( AsParsedText a ) where
  toJSON = Aeson.toJSON . prettyShow . fromAsParsedText
  toEncoding = Aeson.toEncoding . prettyShow . fromAsParsedText
