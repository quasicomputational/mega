module Q4C12.ProjectFile
  ( -- * Core type and lenses to access it.
    Config
  , emptyConfig
  , constraints
  , packages

    -- * Constraints and construction thereof.
  , Constraint
  , constraintBench
  , constraintTest
  , constraintVersion
  , constraintInstalled
  , constraintSource
  , constraintFlag
  , Qualification
  , unqualifiedOnly
  , qualifiedSetup
  , qualifiedSetupAll
  , qualifiedAll

    -- * Consuming constraints
  , constraintPackageName
  , constraintToVersionRange
  , constraintAppliesToUnqualified
  , constraintAppliesToSetup
  , renderConstraint

    -- * Parsing
  , ParseError
  , parse
  , renderError
  )
  where

import Control.Applicative.Combinators
  ( sepBy
  , sepBy1
  )
import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Data.Text as ST
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import Distribution.Types.GenericPackageDescription
  ( FlagName
  , mkFlagName
  , unFlagName
  )
import Distribution.Types.PackageName
  ( PackageName
  , mkPackageName
  , unPackageName
  )
import Distribution.Types.VersionInterval
  ( VersionInterval
  , asVersionIntervals
  , fromVersionIntervals
  , mkVersionIntervals
  , Bound ( ExclusiveBound, InclusiveBound )
  , LowerBound ( LowerBound )
  , UpperBound ( NoUpperBound, UpperBound )
  )
import Distribution.Types.VersionRange
  ( VersionRange
  , anyVersion
  , earlierVersion
  , intersectVersionRanges
  , laterVersion
  , majorBoundVersion
  , noVersion
  , orEarlierVersion
  , orLaterVersion
  , thisVersion
  , unionVersionRanges
  )
import Distribution.Version
  ( Version
  , mkVersion
  , version0
  , versionNumbers
  )
import Q4C12.AesonCabal ()
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as MPE

data Config = Config
  { configConstraints :: Seq Constraint
  , configPackages :: Seq SText
  }
  deriving stock ( Show )

emptyConfig :: Config
emptyConfig = Config
  { configConstraints = mempty
  , configPackages = mempty
  }

constraints :: Lens' Config (Seq Constraint)
constraints f config =
  (\ cs -> config { configConstraints = cs } ) <$> f ( configConstraints config )

packages :: Lens' Config (Seq SText)
packages f config =
  (\ ps -> config { configPackages = ps } ) <$> f ( configPackages config )

data Constraint = Constraint PackageName Qualification PackageConstraint
  deriving stock ( Eq, Generic, Ord, Show )

instance Aeson.FromJSON Constraint
instance Aeson.ToJSON Constraint where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data Qualification
  = UnqualifiedOnly
  | QualifiedAll
  | QualifiedSetupAll
  | QualifiedSetup PackageName
  deriving stock ( Eq, Generic, Ord, Show )

instance Aeson.FromJSON Qualification
instance Aeson.ToJSON Qualification where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

unqualifiedOnly :: Qualification
unqualifiedOnly = UnqualifiedOnly

qualifiedAll :: Qualification
qualifiedAll = QualifiedAll

qualifiedSetupAll :: Qualification
qualifiedSetupAll = QualifiedSetupAll

qualifiedSetup :: PackageName -> Qualification
qualifiedSetup = QualifiedSetup

data LB
  = LBInclusive Version
  | LBExclusive Version
  deriving stock ( Eq, Generic, Ord, Show )

instance Aeson.FromJSON LB
instance Aeson.ToJSON LB where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data UB
  = UBOpen
  | UBInclusive Version
  | UBExclusive Version
  deriving stock ( Eq, Generic, Ord, Show )

instance Aeson.FromJSON UB
instance Aeson.ToJSON UB where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data Interval = Interval LB UB
  deriving stock ( Eq, Generic, Ord, Show )

instance Aeson.FromJSON Interval
instance Aeson.ToJSON Interval where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- TODO: test that fromInterval and toInterval are inverses
-- TODO: offer these as an Iso?
fromInterval :: Interval -> VersionInterval
fromInterval (Interval lb ub) = (lb', ub')
  where
    lb' = case lb of
      LBInclusive v -> LowerBound v InclusiveBound
      LBExclusive v -> LowerBound v ExclusiveBound
    ub' = case ub of
      UBOpen -> NoUpperBound
      UBInclusive v -> UpperBound v InclusiveBound
      UBExclusive v -> UpperBound v ExclusiveBound

toInterval :: VersionInterval -> Interval
toInterval (lb, ub) = Interval lb' ub'
  where
    lb' = case lb of
      LowerBound v InclusiveBound -> LBInclusive v
      LowerBound v ExclusiveBound -> LBExclusive v
    ub' = case ub of
      NoUpperBound -> UBOpen
      UpperBound v InclusiveBound -> UBInclusive v
      UpperBound v ExclusiveBound -> UBExclusive v

data PackageConstraint
  = PackageConstraintVersion [Interval]
  | PackageConstraintTest
  | PackageConstraintBench
  | PackageConstraintSource
  | PackageConstraintInstalled
  | PackageConstraintFlag Bool FlagName
  deriving stock ( Eq, Generic, Ord, Show )

instance Aeson.FromJSON PackageConstraint
instance Aeson.ToJSON PackageConstraint where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

constraintVersion
  :: PackageName
  -> Qualification
  -> VersionRange
  -> Constraint
constraintVersion pn qual
  = Constraint pn qual
  . PackageConstraintVersion
  . fmap toInterval
  . asVersionIntervals

constraintTest
  :: PackageName
  -> Qualification
  -> Constraint
constraintTest pn qual = Constraint pn qual PackageConstraintTest

constraintBench
  :: PackageName
  -> Qualification
  -> Constraint
constraintBench pn qual = Constraint pn qual PackageConstraintBench

constraintSource
  :: PackageName
  -> Qualification
  -> Constraint
constraintSource pn qual = Constraint pn qual PackageConstraintSource

constraintInstalled
  :: PackageName
  -> Qualification
  -> Constraint
constraintInstalled pn qual = Constraint pn qual PackageConstraintInstalled

constraintFlag
  :: PackageName
  -> Qualification
  -> Bool
  -> FlagName
  -> Constraint
constraintFlag pn qual enabled flag =
  Constraint pn qual ( PackageConstraintFlag enabled flag )

constraintToVersionRange
  :: Constraint
  -> VersionRange
constraintToVersionRange ( Constraint _ _ inner ) = case inner of
  PackageConstraintVersion intervals ->
    fromVersionIntervals $ mkVersionIntervals $ fromInterval <$> intervals
  _ ->
    anyVersion

constraintPackageName :: Constraint -> PackageName
constraintPackageName ( Constraint pn _ _ ) = pn

constraintAppliesToUnqualified :: Constraint -> Bool
constraintAppliesToUnqualified (Constraint _ qual _) = case qual of
  UnqualifiedOnly -> True
  QualifiedAll -> True
  QualifiedSetupAll -> False
  QualifiedSetup _ -> False

constraintAppliesToSetup :: PackageName -> Constraint -> Bool
constraintAppliesToSetup pkg (Constraint _ qual _) = case qual of
  UnqualifiedOnly -> False
  QualifiedAll -> True
  QualifiedSetupAll -> True
  QualifiedSetup pkg' -> pkg == pkg'

data ParseError
  = ParseError SText

renderError :: ParseError -> SText
renderError ( ParseError err ) = err

--TODO thread through the source file name
parse :: SText -> Either ParseError Config
parse = first prettify . MP.runParser parser "(input)"
  where
    prettify :: MPE.ParseErrorBundle SText Void -> ParseError
    prettify = ParseError . ST.pack . MPE.errorBundlePretty

parser :: (Ord e) => MP.Parsec e SText Config
parser = do
  spaceNL
  endos <- many $ pPackages <|> pConstraints <|> pUnknown
  MP.eof
  pure $ appEndo (fold endos) $ Config mempty mempty

lineComment :: (Ord e) => MP.Parsec e SText ()
lineComment = Lexer.skipLineComment "--"

spaceNL :: (Ord e) => MP.Parsec e SText ()
spaceNL = Lexer.space MPC.space1 lineComment empty

space :: (Ord e) => MP.Parsec e SText ()
space = Lexer.space (void $ MP.takeWhile1P Nothing f) lineComment empty
  where
    f x = isSpace x && x /= '\n'

ensureIndented :: (Ord e) => MP.Parsec e SText ()
ensureIndented = do
  spaceNL
  actual <- Lexer.indentLevel
  unless (actual > MP.pos1) $
    fail $ "continuation line not indented"

pPackages :: (Ord e) => MP.Parsec e SText (Endo Config)
pPackages = Lexer.nonIndented spaceNL $ do
  void $ MPC.string "packages:"
  pkgs <- many $ MP.try $ do
    ensureIndented
    MP.takeWhile1P Nothing $ not . isSpace
  spaceNL
  pure $ Endo $ packages <>~ Seq.fromList pkgs

pConstraints :: (Ord e) => MP.Parsec e SText (Endo Config)
pConstraints = Lexer.nonIndented spaceNL $ do
  void $ MPC.string "constraints:"
  cstrss <- sepBy singleConstraint ( MP.try $ ensureIndented *> MPC.string "," )
  spaceNL
  pure $ Endo $ constraints <>~ foldMap Seq.fromList cstrss
  where
    singleConstraint :: (Ord e) => MP.Parsec e SText [Constraint]
    singleConstraint = MP.try $ do
      ensureIndented
      constraintFn <- pQualPkg
      ensureIndented
      fmap constraintFn <$> pPackageConstraints

pQualPkg :: (Ord e) => MP.Parsec e SText (PackageConstraint -> Constraint)
pQualPkg = anyQual <|> anySetup <|> MP.try specificSetup <|> unqual
  where

    unqual :: (Ord e) => MP.Parsec e SText (PackageConstraint -> Constraint)
    unqual = do
      pkg <- pPackageName
      pure $ Constraint pkg UnqualifiedOnly

    anyQual :: (Ord e) => MP.Parsec e SText (PackageConstraint -> Constraint)
    anyQual = do
      pkg <- MPC.string "any." *> pPackageName
      pure $ Constraint pkg QualifiedAll

    anySetup :: (Ord e) => MP.Parsec e SText (PackageConstraint -> Constraint)
    anySetup = do
      pkg <- MPC.string "setup." *> pPackageName
      pure $ Constraint pkg QualifiedSetupAll

    specificSetup :: (Ord e) => MP.Parsec e SText (PackageConstraint -> Constraint)
    specificSetup = do
      setupPkg <- pPackageName
      void $ MPC.string ":setup."
      pkg <- pPackageName
      pure $ Constraint pkg (QualifiedSetup setupPkg)

--TODO: this is looser than the spec, which is defined at <https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-name>
pPackageName :: (Ord e) => MP.Parsec e SText PackageName
pPackageName = mkPackageName . ST.unpack <$> MP.takeWhileP Nothing validChar
  where
    validChar c = isAlphaNum c || c == '-'

pPackageConstraints :: (Ord e) => MP.Parsec e SText [PackageConstraint]
pPackageConstraints
    = fmap pure pVersionConstraint
  <|> fmap pure pTestConstraint
  <|> fmap pure pBenchConstraint
  <|> fmap pure pSourceConstraint
  <|> fmap pure pInstalledConstraint
  <|> pFlagConstraints

pVersionConstraint :: (Ord e) => MP.Parsec e SText PackageConstraint
pVersionConstraint = fmap (PackageConstraintVersion . fmap toInterval . asVersionIntervals) $
      MP.try pUnionVersionRanges
  <|> MP.try pIntersectVersionRanges
  <|> pVersionRangeAtom

pVersionRangeAtom :: (Ord e) => MP.Parsec e SText VersionRange
pVersionRangeAtom
    = pThisVersion
  <|> MP.try pEarlierVersion
  <|> pOrEarlierVersion
  <|> MP.try pLaterVersion
  <|> pOrLaterVersion
  <|> pMajorVersion
  <|> MP.try pAnyVersion
  <|> MP.try pNoVersion

pThisVersion :: (Ord e) => MP.Parsec e SText VersionRange
pThisVersion = do
  void $ MPC.string "=="
  space
  thisVersion <$> pVersion

pEarlierVersion :: (Ord e) => MP.Parsec e SText VersionRange
pEarlierVersion = do
  void $ MPC.string "<"
  space
  earlierVersion <$> pVersion

pOrEarlierVersion :: (Ord e) => MP.Parsec e SText VersionRange
pOrEarlierVersion = do
  void $ MPC.string "<="
  space
  orEarlierVersion <$> pVersion

pLaterVersion :: (Ord e) => MP.Parsec e SText VersionRange
pLaterVersion = do
  void $ MPC.string ">"
  space
  laterVersion <$> pVersion

pOrLaterVersion :: (Ord e) => MP.Parsec e SText VersionRange
pOrLaterVersion = do
  void $ MPC.string ">="
  space
  orLaterVersion <$> pVersion

pMajorVersion :: (Ord e) => MP.Parsec e SText VersionRange
pMajorVersion = do
  void $ MPC.string "^>="
  space
  majorBoundVersion <$> pVersion

pAnyVersion :: (Ord e) => MP.Parsec e SText VersionRange
pAnyVersion = do
  void $ MPC.string "-any"
  pure anyVersion

pNoVersion :: (Ord e) => MP.Parsec e SText VersionRange
pNoVersion = do
  void $ MPC.string "-none"
  pure noVersion

pIntersectVersionRanges :: (Ord e) => MP.Parsec e SText VersionRange
pIntersectVersionRanges = do
  a <- pVersionRangeAtom
  space
  void $ MPC.string "&&"
  space
  b <- pVersionRangeAtom
  pure $ intersectVersionRanges a b

pUnionVersionRanges :: (Ord e) => MP.Parsec e SText VersionRange
pUnionVersionRanges = do
  a <- MP.try pIntersectVersionRanges <|> pVersionRangeAtom
  space
  void $ MPC.string "||"
  space
  b <- MP.try pIntersectVersionRanges <|> pVersionRangeAtom
  pure $ unionVersionRanges a b

pVersion :: (Ord e) => MP.Parsec e SText Version
pVersion = fmap mkVersion $ sepBy1 Lexer.decimal (MPC.string ".")

pFlagConstraints :: (Ord e) => MP.Parsec e SText [PackageConstraint]
pFlagConstraints = many $ do
  setting <- (MPC.string "+" *> pure True) <|> (MPC.string "-" *> pure False)
  -- <https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-section-flag-flag>
  nameFirst <- MPC.alphaNumChar <|> MPC.char '-'
  nameRest <- MP.takeWhileP Nothing $
    \ c -> isAlphaNum c || c == '_' || c == '-'
  space
  pure $ PackageConstraintFlag setting (mkFlagName $ nameFirst : ST.unpack nameRest)

pBenchConstraint :: (Ord e) => MP.Parsec e SText PackageConstraint
pBenchConstraint = do
  void $ MPC.string "bench"
  pure PackageConstraintBench

pTestConstraint :: (Ord e) => MP.Parsec e SText PackageConstraint
pTestConstraint = do
  void $ MPC.string "test"
  pure PackageConstraintTest

pSourceConstraint :: (Ord e) => MP.Parsec e SText PackageConstraint
pSourceConstraint = do
  void $ MPC.string "source"
  pure PackageConstraintSource

pInstalledConstraint :: (Ord e) => MP.Parsec e SText PackageConstraint
pInstalledConstraint = do
  void $ MPC.string "installed"
  pure PackageConstraintInstalled

-- Parse an entire unknown stanza.
pUnknown :: (Ord e, Monoid a) => MP.Parsec e SText a
pUnknown = Lexer.nonIndented spaceNL $ do
  someLine
  spaceNL
  void $ many $ do
    void $ Lexer.indentGuard spaceNL GT MP.pos1
    someLine
    spaceNL
  pure mempty
  where
    someLine :: (Ord e) => MP.Parsec e SText ()
    someLine = void $ MP.takeWhile1P Nothing $ not . isSpace

renderConstraint :: Constraint -> TBuilder
renderConstraint ( Constraint pkg qual constr ) = fold
  [ case qual of
      UnqualifiedOnly -> ""
      QualifiedAll -> "any."
      QualifiedSetupAll -> "setup."
      QualifiedSetup pkg' -> fold
        [ LTB.fromString $ unPackageName pkg'
        , ":setup."
        ]
  , LTB.fromString $ unPackageName pkg
  , " "
  , case constr of
      PackageConstraintVersion intervals -> case intervals of
        [] ->
          "-none"
        _ ->
          intercalateMap0 " || " (renderVersionInterval . fromInterval) intervals
      PackageConstraintTest -> "test"
      PackageConstraintBench -> "bench"
      PackageConstraintInstalled -> "installed"
      PackageConstraintSource -> "source"
      PackageConstraintFlag True flag -> fold
        [ "+"
        , LTB.fromString $ unFlagName flag
        ]
      PackageConstraintFlag False flag -> fold
        [ "-"
        , LTB.fromString $ unFlagName flag
        ]
  ]

renderVersionInterval :: VersionInterval -> TBuilder
renderVersionInterval (LowerBound ver bound, NoUpperBound)
  | ver == version0
  = "-any"
  | otherwise
  = renderLowerBound ver bound
renderVersionInterval (LowerBound ver bound, UpperBound ver' bound')
  | ver == version0
  = renderUpperBound ver' bound'
  | ver == ver'
  = fold
    [ "== "
    , renderVersion ver
    ]
  | otherwise
  = fold
    [ renderLowerBound ver bound
    , " && "
    , renderUpperBound ver' bound'
    ]

renderLowerBound :: Version -> Bound -> TBuilder
renderLowerBound ver bound = fold
  [ case bound of
      InclusiveBound -> ">="
      ExclusiveBound -> ">"
  , " "
  , renderVersion ver
  ]

renderUpperBound :: Version -> Bound -> TBuilder
renderUpperBound ver bound = fold
  [ case bound of
      InclusiveBound -> "<="
      ExclusiveBound -> "<"
  , " "
  , renderVersion ver
  ]

renderVersion :: Version -> TBuilder
renderVersion = intercalateMap0 "." LTBI.decimal . versionNumbers
