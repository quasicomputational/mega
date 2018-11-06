-- TODO once 8.4 is out of the window, put this in the default extensions
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | (Locally) canonical orphan instances for types from Cabal and classes from aeson.
module Q4C12.AesonCabal
  (
  )
  where

import qualified Data.Aeson as Aeson
import qualified Data.Text as ST
import Distribution.Compiler
  ( CompilerFlavor
  )
import Distribution.PackageDescription
  ( FlagAssignment
  , FlagName
  , mkFlagAssignment
  , unFlagAssignment
  )
import Distribution.Parsec.Class
  ( Parsec
  , eitherParsec
  )
import Distribution.Pretty
  ( Pretty
  , prettyShow
  )
import Distribution.System
  ( Arch
  , OS
  )
import Distribution.Types.PackageName
  ( PackageName
  )
import Distribution.Version
  ( Version
  )

-- TODO: round-trip tests

-- Instances that use Pretty/Parsec to round-trip.

newtype AsParsedText a = AsParsedText { fromAsParsedText :: a }

instance ( Parsec a ) => Aeson.FromJSON ( AsParsedText a ) where
  parseJSON = Aeson.withText "Cabal value" $
    either fail ( pure . AsParsedText ) . eitherParsec . ST.unpack

instance ( Pretty a ) => Aeson.ToJSON ( AsParsedText a ) where
  toJSON = Aeson.toJSON . prettyShow . fromAsParsedText
  toEncoding = Aeson.toEncoding . prettyShow . fromAsParsedText

deriving via (AsParsedText Arch) instance Aeson.FromJSON Arch
deriving via (AsParsedText CompilerFlavor) instance Aeson.FromJSON CompilerFlavor
deriving via (AsParsedText FlagName) instance Aeson.FromJSON FlagName
deriving via (AsParsedText OS) instance Aeson.FromJSON OS
deriving via (AsParsedText PackageName) instance Aeson.FromJSON PackageName
deriving via (AsParsedText Version) instance Aeson.FromJSON Version

deriving via (AsParsedText Arch) instance Aeson.ToJSON Arch
deriving via (AsParsedText CompilerFlavor) instance Aeson.ToJSON CompilerFlavor
deriving via (AsParsedText FlagName) instance Aeson.ToJSON FlagName
deriving via (AsParsedText OS) instance Aeson.ToJSON OS
deriving via (AsParsedText PackageName) instance Aeson.ToJSON PackageName
deriving via (AsParsedText Version) instance Aeson.ToJSON Version

-- Other instances

instance Aeson.FromJSON FlagAssignment where
  parseJSON = fmap mkFlagAssignment . Aeson.parseJSON

instance Aeson.ToJSON FlagAssignment where
  toJSON = Aeson.toJSON . unFlagAssignment
  toEncoding = Aeson.toEncoding . unFlagAssignment
