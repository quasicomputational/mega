{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
module Q4C12.XML
  ( Element (Element), element
  , QName (QName)
  , DoctypeResolver, systemResolver, publicResolver, noEntities
  , Markup (Markup, getMarkup), markupNull
  , markupElement, markupText, markupSText, markupBuilder
  , htmlNS, xmlNS, svgNS, mathmlNS
  , hname, xname, rngname, uname, rngelem, helem
  , addUAttr, addUAttrS, addAttr, addAttrS, addAttrs
  , XMLError, displayError
  , XMLWarning, displayWarnings
  , isXMLSpace
  , parseXML
  , renderXML
  )
  where

import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State (get, gets, put, modify)
import Control.Monad.Trans.Writer (tell)
import qualified Data.DList as DList
import qualified Data.DList.NonEmpty as NEDList
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as ST
import Data.Text.ICU.Normalize (isNormalized, NormalizationMode (NFD))
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Formatting (bprint, stext, Format, later, int, builder)
import Q4C12.MapPend (MapPend, getMapPend)
import qualified Q4C12.MapPend as MapPend
import Q4C12.Position (Position, startPosition, PositionRange (PositionRange), updatePosition, position, positionRange)
import Q4C12.TwoFinger (singletonOddA, onlyOddA, unitOddA, consOddA)
import Safe (toEnumMay)

--TODO: split text chunks by XML newline and give each chunk its own PositionRange, to make tracking locations in files where end-of-line normalisation has been applied easier.

data QName = QName !(Maybe SText) !SText
  deriving (Show, Eq, Ord, Generic)

instance NFData QName

htmlNS :: SText
htmlNS = "http://www.w3.org/1999/xhtml"

hname :: SText -> QName
hname = QName (Just htmlNS)

xmlNS :: SText
xmlNS = "http://www.w3.org/XML/1998/namespace"

xname :: SText -> QName
xname = QName (Just xmlNS)

rngname :: SText -> QName
rngname = QName (Just "http://relaxng.org/ns/structure/1.0")

mathmlNS :: SText
mathmlNS = "http://www.w3.org/1998/Math/MathML"

svgNS :: SText
svgNS = "http://www.w3.org/2000/svg"

uname :: SText -> QName
uname = QName Nothing

isXMLSpace :: Char -> Bool
isXMLSpace ' ' = True
isXMLSpace '\t' = True
isXMLSpace '\n' = True
isXMLSpace '\r' = True
isXMLSpace _ = False

newtype Markup pos = Markup { getMarkup :: TwoFingerOddA (Element pos) (Seq (pos, LText)) }
  deriving (Show, Generic)

instance (NFData pos) => NFData (Markup pos)

instance Functor Markup where
  fmap f (Markup ilv) = Markup $ bimap (fmap f) (fmap $ first f) ilv

instance Semigroup (Markup pos) where
  Markup a <> Markup b = Markup $ a <> b

instance Monoid (Markup pos) where
  mempty = Markup mempty
  mappend = (<>)

markupNull :: Markup pos -> Bool
markupNull (Markup tree) = maybe False null (onlyOddA tree)

--TODO: shouldn't attr values be strict?
--TODO: source location for attrs
data Element pos = Element
  { _ename :: QName
  , _eattrs :: Map QName (pos, Seq (pos, LText))
  , _ebody :: Markup pos
  , _epos :: pos
  }
  deriving (Show, Functor, Generic)

instance (NFData pos) => NFData (Element pos)

eattrs :: Lens' (Element pos) (Map QName (pos, Seq (pos, LText)))
eattrs f (Element n attrs body pos) = f attrs <&>
  \attrs' -> Element n attrs' body pos

markupElement :: Element pos -> Markup pos
markupElement = Markup . unitOddA

markupTextPos :: pos -> LText -> Markup pos
markupTextPos pos t = Markup $ singletonOddA $ Seq.singleton (pos, t)

markupText :: LText -> Markup ()
markupText = markupTextPos ()

markupBuilder :: TBuilder -> Markup ()
markupBuilder = markupText . LTB.toLazyText

markupSText :: SText -> Markup ()
markupSText = markupSTextPos ()

markupSTextPos :: pos -> SText -> Markup pos
markupSTextPos pos = markupTextPos pos . LT.fromStrict

element :: QName -> Markup () -> Element ()
element qname markup = Element qname mempty markup ()

--mappends on the left, to take priority
addAttrs :: Map QName LText -> Element () -> Element ()
addAttrs attrs = eattrs <>~ (attrs <&> \a -> ((), Seq.singleton ((), a)))

at :: (Ord k, Functor f) => k -> (Maybe v -> f (Maybe v)) -> Map k v -> f (Map k v)
at = flip Map.alterF

addAttr :: QName -> LText -> Element () -> Element ()
addAttr qname val = set (eattrs . at qname) (Just ((), Seq.singleton ((), val)))

addAttrS :: QName -> SText -> Element () -> Element ()
addAttrS qname = addAttr qname . LT.fromStrict

addUAttrS :: SText -> SText -> Element () -> Element ()
addUAttrS name = addUAttr name . LT.fromStrict

addUAttr :: SText -> LText -> Element () -> Element ()
addUAttr = addAttr . uname

rngelem :: SText -> Markup () -> Element ()
rngelem = element . rngname

helem :: SText -> Markup () -> Element ()
helem = element . hname

renderXML :: Element pos -> TBuilder
renderXML = execWriter . renderElement

renderElement :: Element pos -> Writer TBuilder ()
renderElement (Element name attrs (Markup body) _) = flip evalStateT 1 $ do
  lift $ tell "<"
  cur <- get
  lift $ renderQName cur name
  renderNamespaceFor name
  for_ (Map.toAscList attrs) $ \(attrName, (_, chunks)) -> do
    lift $ tell " "
    n <- get
    lift $ renderQName n attrName
    lift $ tell "=\""
    lift $ traverse_ (renderAttrValue . snd) chunks
    lift $ tell "\""
    renderNamespaceFor attrName
  lift $ tell ">"
  lift $ bitraverse_ renderElement (traverse_ $ traverse_ renderContent) body
  lift $ tell "</"
  lift $ renderQName cur name
  lift $ tell ">"
  where
    renderQName :: Word -> QName -> Writer TBuilder ()
    renderQName _ (QName Nothing local) =
      tell $ LTB.fromText local
    renderQName _ (QName (Just ns) local) | ns == xmlNS =
      tell $ bprint ("xml:" . stext) local
    renderQName cur (QName (Just _) local) =
      tell $ bprint ("n" . int . ":" . stext) cur local
    renderNamespaceFor :: QName -> StateT Word (Writer TBuilder) ()
    renderNamespaceFor (QName Nothing _) = pure ()
    renderNamespaceFor (QName (Just ns) _) | ns == xmlNS =
      pure ()
    renderNamespaceFor (QName (Just ns) _) = do
      cur <- get
      lift $ tell $ bprint (" xmlns:n" . int . "=\"") cur
      lift $ renderAttrValue $ LT.fromStrict ns
      lift $ tell "\""
      modify (+ 1)

renderContent :: LText -> Writer TBuilder ()
renderContent = tell . LTB.fromText . ST.replace "\r" "&#xD;" . ST.replace "<" "&lt;" . ST.replace ">" "&gt;" . ST.replace "&" "&amp;" . LT.toStrict

renderAttrValue :: LText -> Writer TBuilder ()
renderAttrValue = tell . LTB.fromText . ST.replace "\n" "&#xA;" . ST.replace "\r" "&#xD;" . ST.replace "\t" "&#x9;" . ST.replace "\"" "&quot;" . ST.replace "<" "&lt;" . ST.replace ">" "&gt;" . ST.replace "&" "&amp;" . LT.toStrict

--(prefix, local); this is used before we have mapped prefices to namespaces.
data RawQName = RawQName (Maybe SText) SText
  deriving (Eq, Show, Generic)

instance NFData RawQName

rawQName :: Format r (RawQName -> r)
rawQName = later $ \case
  RawQName Nothing local -> bprint stext local
  RawQName (Just pref) local -> bprint (stext . ":" . stext) pref local

--TODO: it'd be nice to be able to mark individual entities as deprecated, wouldn't it?
--TODO: factor the various 'stray X at Y' errors together?
data XMLError
  = UnmatchedTags Position RawQName Position RawQName
  | TextPreDoctype Position
  | TextPostDoctype Position
  | TrailingText Position
  | StrayDoctype Position
  | TrailingStartTag Position Position
  | TrailingEndTag Position Position
  | TrailingRef Position
  | NoRoot
  | PostDoctypeEndTag Position
  | PostDoctypeRef Position
  | PreDoctypeEndTag Position
  | PreDoctypeRef Position
  | XMLBadSyntax SText Position
  | XMLUnexpectedEoF SText
  | XMLNonChar Position
  | BadCharRef PositionRange
  | NonNFDName PositionRange
  | XMLStrayGT Position
  | ResolveError (NonEmpty ResolveError)
  | DoctypeRootNameMismatch Position PositionRange
  deriving (Show, Generic)

instance NFData XMLError

data ResolveError
  = UnknownEntity PositionRange SText
  | UnboundNamespace PositionRange SText
  | DuplicateAttrs PositionRange (NonEmpty PositionRange)
  | DuplicateNamespacePrefixDecls SText PositionRange (NonEmpty PositionRange)
  | DuplicateDefaultNamespaceDecls PositionRange (NonEmpty PositionRange)
  | NonNFDNamespace PositionRange
  deriving (Show, Generic)

instance NFData ResolveError

data XMLWarning
  = UnknownDoctype DOCTYPE Position
  | AttributeNormalisation Position
  deriving (Show, Generic)

instance NFData XMLWarning

displayResolveError :: ResolveError -> TBuilder
displayResolveError (UnknownEntity pos name) = bprint
  --TODO: escape the entity name??
  ("Error: Unknown entity '" . stext . "' at " . positionRange . ".")
  name pos
displayResolveError (UnboundNamespace pos pref) = bprint
  ("Error: Unknown namespace prefix '" . stext . "' at " . positionRange . ".")
  pref pos
displayResolveError (DuplicateAttrs r0 rs) =
  bprint ("Error: Duplicate attributes at " . builder . " and " . positionRange . ".") (intercalateMap0 ", " (bprint positionRange) rs) r0
displayResolveError (DuplicateNamespacePrefixDecls pref r0 rs) =
  bprint ("Error: Duplicate definitions for prefix '" . stext . "' at " . builder . " and " . positionRange . ".") pref (intercalateMap0 ", " (bprint positionRange) rs) r0
displayResolveError (DuplicateDefaultNamespaceDecls r0 rs) =
  bprint ("Error: Duplicate default namespace definitions at " . builder . " and " . positionRange . ".") (intercalateMap0 ", " (bprint positionRange) rs) r0
displayResolveError (NonNFDNamespace r) = bprint
  ("Error: Namespace is not in NFD at " . positionRange . ".")
  r

displayError :: XMLError -> TBuilder
displayError (UnmatchedTags opos open cpos close) = bprint
  ( "Error: Misnested tags: Open tag is '" . rawQName . "' at " . position
  . ", but close tag is '" . rawQName . "' at " . position . "."
  )
  open opos close cpos
displayError (TextPreDoctype pos) = bprint
  ("Error: Unexpected text at " . position . " before the doctype.")
  pos
displayError (TextPostDoctype pos) = bprint
  ("Error: Unexpected text at " . position . " before the root element.")
  pos
displayError (TrailingText pos) = bprint
  ("Error: Unexpected text at " . position . " after the root element.")
  pos
displayError (StrayDoctype pos) =
  bprint ("Error: Stray doctype at " . position . ".") pos
displayError (TrailingEndTag rootClosePos tagPos) = bprint
  ( "Error: Trailing closing tag at " . position
  . "; the root element was closed at " . position . "."
  )
  tagPos rootClosePos
displayError (TrailingStartTag rootClosePos tagPos) = bprint
  ( "Error: Trailing opening tag at " . position
  . "; the root element was closed at " . position . "."
  )
  tagPos rootClosePos
displayError (TrailingRef pos) = bprint
  ("Error: Reference at " . position . ", after the root element.")
  pos
displayError NoRoot = "Error: Missing root element."
displayError (PostDoctypeEndTag pos) = bprint
  ("Error: Close tag follows doctype at " . position . ".")
  pos
displayError (PostDoctypeRef pos) = bprint
  ("Error: Reference at " . position . ", before the root element.")
  pos
displayError (PreDoctypeEndTag pos) = bprint
  ("Error: Close tag seen at " . position . ", before the doctype.")
  pos
displayError (PreDoctypeRef pos) = bprint
  ("Error: Reference at " . position . ", before the doctype.")
  pos
displayError (ResolveError errs) =
  intercalateMap0 "\n" displayResolveError errs
displayError (XMLUnexpectedEoF instead) = bprint
  ("Error: End of input seen, instead of expected " . stext . ".")
  instead
displayError (XMLBadSyntax msg pos) = bprint
  ("Error: Bad syntax: " . stext . " at " . position . ".")
  msg pos
displayError (XMLNonChar pos) = bprint
  ("Error: Non-XML character at " . position . ".")
  pos
displayError (BadCharRef pos) = bprint
  ("Error: Character reference is not an XML character at " . positionRange . ".")
  pos
displayError (XMLStrayGT pos) =
  bprint ("Error: Stray '>' at " . position . ".") pos
displayError (NonNFDName pos) = bprint
  ("Error: Non-NFD attribute or element name at " . positionRange . ".")
  pos
displayError (DoctypeRootNameMismatch doctypePos rootPosRange) = bprint
  ("Error: Doctype (at " . position . ") and root element (at " . positionRange . ") name mis-match.")
  doctypePos rootPosRange

displayWarnings :: (Foldable f) => f XMLWarning -> TBuilder
displayWarnings = foldMap (\warn -> displayWarning warn <> "\n")

--TODO: a -Werror option, which would mean prefixing this with 'error', not 'warning'.
--TODO: escape the name?
displayWarning :: XMLWarning -> TBuilder
displayWarning (UnknownDoctype (DOCTYPEPublic pub sys) pos) = bprint
  ("Warning: The doctype with public identifier " . stext . " and system identifier \"" . stext . "\" at " . position . " is not recognised.")
  pub sys pos
displayWarning (UnknownDoctype (DOCTYPESystem sys) pos) = bprint
  ("Warning: The doctype with system identifier \"" . stext . "\" at " . position . " is not recognised.")
  sys pos
displayWarning (AttributeNormalisation pos) = bprint
  ("Warning: Performing attribute normalisation at " . position . ".")
  pos

data UElement = UElement RawQName [(RawQName, PositionRange, UContent)] (TwoFingerOddA UElement UContent) PositionRange

newtype UContent = UContent (TwoFingerOddA (PositionRange, SText) (Seq (PositionRange, SText)))

instance Semigroup UContent where
  UContent a <> UContent b = UContent (a <> b)

instance Monoid UContent where
  mempty = UContent mempty
  mappend = (<>)

resolveContent :: (SText -> Maybe LText) -> UContent -> Validate (NonEmptyDList ResolveError) (Seq (PositionRange, LText))
resolveContent entity (UContent parts) = bifoldMapM resolveEntity (pure . fmap (fmap LT.fromStrict)) parts
  where
    resolveEntity (pos, "amp") = pure $ Seq.singleton (pos, "&")
    resolveEntity (pos, "gt") = pure $ Seq.singleton (pos, ">")
    resolveEntity (pos, "lt") = pure $ Seq.singleton (pos, "<")
    resolveEntity (pos, "quot") = pure $ Seq.singleton (pos, "\"")
    resolveEntity (pos, "apos") = pure $ Seq.singleton (pos, "'")
    resolveEntity (pos, name) = case entity name of
      Nothing -> failure $ NEDList.singleton $ UnknownEntity pos name
      Just ent -> pure $ Seq.singleton (pos, ent)

resolveElement :: (SText -> Maybe LText) -> Maybe SText -> Map SText SText -> UElement -> Validate (NonEmptyDList ResolveError) (Element PositionRange)
resolveElement entity defaultNamespace namespaces (UElement rawName rawAttrs rawBody pos) = exceptToValidate $ do
  --TODO: can do more of this in parallel and report more errors at once than we currently are.
  rawAttrs' <- validateToExcept $ for rawAttrs $ \(rawAttrName, attrPos, rawContent) ->
    (,,) rawAttrName attrPos <$> resolveContent entity rawContent
  let prepartitioned :: [HSum '[(PositionRange, SText), MapPend SText (NonEmpty (PositionRange, SText)), (PositionRange, RawQName, Seq (PositionRange, LText))]]
      prepartitioned = flip fmap rawAttrs' $ \(RawQName mpref local, nameRange, value) ->
        case mpref of
          Nothing | local == "xmlns" -> HSumHere (nameRange, LT.toStrict $ foldMap snd value)
          Just "xmlns" -> HSumThere $ HSumHere $ MapPend.singleton local $ pure (nameRange, LT.toStrict $ foldMap snd value)
          _ -> HSumThere $ HSumThere $ HSumHere (nameRange, RawQName mpref local, value)
  case partitionHSum prepartitioned of
    HProdListCons defaultNamespaces (HProdListCons collatedNamespaceBindings (HProdListCons otherAttrs HProdListNil)) -> do
      defaultNamespace' <- validateToExcept $ case defaultNamespaces of
        [] -> pure defaultNamespace
        (_, "") : [] -> pure Nothing
        (_, ns) : [] | isNormalized NFD ns -> pure (Just ns)
        (r0, _) : [] -> failure $ NEDList.singleton $ NonNFDNamespace r0
        (r0, _) : (r1, _) : rest -> failure $ NEDList.singleton $ DuplicateDefaultNamespaceDecls r0 (r1 :| fmap fst rest)
      namespaceBindings <- validateToExcept $ flip Map.traverseWithKey (getMapPend $ fold collatedNamespaceBindings) $ \pref -> \case
        (_, ns) :| [] | isNormalized NFD ns -> pure ns
        (r, _) :| [] -> failure $ NEDList.singleton $ NonNFDNamespace r
        (r0, _) :| ((r1, _) : rest) ->
          failure $ NEDList.singleton $ DuplicateNamespacePrefixDecls pref r0 (r1 :| fmap fst rest)
      let namespaces' = namespaceBindings <> namespaces --Note: new on the left, to take precedence.
      qname <- validateToExcept $ case rawName of
        RawQName Nothing local -> pure $ QName defaultNamespace' local
        RawQName (Just pref) local -> case Map.lookup pref namespaces' of
          Nothing -> failure $ NEDList.singleton $ UnboundNamespace pos pref
          Just ns -> pure $ QName (Just ns) local
      attrSets <- validateToExcept $ fmap getMapPend $
        flip foldMapM otherAttrs $ \(r, RawQName mpref local, value) ->
          case mpref of
            Nothing -> pure $ MapPend.singleton (QName Nothing local) $ pure (r, value)
            Just pref -> case Map.lookup pref namespaces' of
              Nothing -> failure $ NEDList.singleton $ UnboundNamespace r pref
              Just ns -> pure $ MapPend.singleton (QName (Just ns) local) $ pure (r, value)
      attrs <- validateToExcept $ for attrSets $ \case
        (r, value) :| [] -> pure (r, value)
        (r0, _) :| ((r1, _) : rest) -> failure $ NEDList.singleton $ DuplicateAttrs r0 (r1 :| fmap fst rest)
      body <- validateToExcept $
        bitraverse (resolveElement entity defaultNamespace' namespaces') (resolveContent entity) rawBody
      pure $ Element qname attrs (Markup body) pos

--TODO: this is very simplistic, but it should do the job. Might think about maybe branching out to XML catalogs, but this is fine.
data DoctypeResolver
  = DoctypeResolverSystem (Map SText (SText -> Maybe LText))
  | DoctypeResolverPublic (Map SText (SText -> Maybe LText))

systemResolver :: Map SText (SText -> Maybe LText) -> DoctypeResolver
systemResolver = DoctypeResolverSystem

publicResolver :: Map SText (SText -> Maybe LText) -> DoctypeResolver
publicResolver = DoctypeResolverPublic

noEntities :: DoctypeResolver
noEntities = DoctypeResolverSystem mempty

parseXML :: DoctypeResolver -> SText -> (Either XMLError (Element PositionRange), [XMLWarning])
parseXML doctypeResolver input = fmap toList $ runWriter $ runExceptT $ evalStateT (toplevel doctypeResolver) (input, startPosition)

toplevel :: DoctypeResolver -> Parser (Element PositionRange)
toplevel doctypeResolver = do
  void xmlSpaces
  (input, pos) <- get
  when (ST.null input) $
    lift $ throwE NoRoot
  chooseFrom (lift $ throwE $ TextPreDoctype pos)
    [ ("&", lift $ throwE $ PreDoctypeRef pos)
    , ("<", do
        res <- xmlSeenLT pos
        case res of
          SeenLTElement e -> do
            endPos <- gets snd
            finish endPos
            lift $ withExceptT (ResolveError . toNonEmpty) $ validateToExcept $ resolveElement (const Nothing) Nothing defaultNamespaces e
          SeenLTComment -> toplevel doctypeResolver
          SeenLTSlash -> lift $ throwE $ PreDoctypeEndTag pos
          SeenLTDOCTYPE rootName Nothing -> seenDoctype pos rootName (const Nothing)
          SeenLTDOCTYPE rootName (Just doctype) -> case doctypeResolver of
            DoctypeResolverSystem doctypeMap
              | Just entityFunc <- Map.lookup (getSystemId doctype) doctypeMap
              -> seenDoctype pos rootName entityFunc
            DoctypeResolverPublic doctypeMap
              | DOCTYPEPublic pub _ <- doctype
              , Just entityFunc <- Map.lookup pub doctypeMap
              -> seenDoctype pos rootName entityFunc
            _ -> do
              lift $ lift $ tell $ DList.singleton $ UnknownDoctype doctype pos
              seenDoctype pos rootName (const Nothing))
    ]
  where
    defaultNamespaces :: Map SText SText
    defaultNamespaces = Map.fromList
      [ ("xml", xmlNS)
      ]
    seenDoctype doctypePos rootName entityFunc = do
      void xmlSpaces
      (input, pos) <- get
      when (ST.null input) $
        lift $ throwE NoRoot
      chooseFrom (lift $ throwE $ TextPostDoctype pos)
        [ ("&", lift $ throwE $ PostDoctypeRef pos)
        , ("<", do
            res <- xmlSeenLT pos
            case res of
              SeenLTElement e -> do
                case e of
                  UElement name _ _ posRange -> unless (rootName == name) $
                    lift $ throwE $ DoctypeRootNameMismatch doctypePos posRange
                finish pos
                lift $ withExceptT (ResolveError . toNonEmpty) $ validateToExcept $ resolveElement entityFunc Nothing defaultNamespaces e
              SeenLTComment -> seenDoctype doctypePos rootName entityFunc
              SeenLTSlash -> lift $ throwE $ PostDoctypeEndTag pos
              SeenLTDOCTYPE _ _ -> lift $ throwE $ StrayDoctype pos)
        ]
    finish rootPos = do
      void xmlSpaces
      (input, pos) <- get
      unless (ST.null input) $
        chooseFrom (lift $ throwE $ TrailingText pos)
          [ ("&", lift $ throwE $ TrailingRef pos)
          , ("<", do
              res <- xmlSeenLT pos
              case res of
                SeenLTElement _ -> lift $ throwE $ TrailingStartTag rootPos pos
                SeenLTComment -> finish rootPos
                SeenLTSlash -> lift $ throwE $ TrailingEndTag rootPos pos
                SeenLTDOCTYPE _ _ -> lift $ throwE $ StrayDoctype pos)
          ]

--TODO: give a way for the doctype lookup function to signal that a doctype is recognised but deprecated.
--Note order of transformers: with ExceptT outside Writer, we will always report warnings alongside the error.
type Parser = StateT (SText, Position) (ExceptT XMLError (Writer (DList XMLWarning)))

xmlSpaces :: Parser Bool
xmlSpaces = do
  (input, startPos) <- get
  let (spaces, input') = ST.span isXMLSpace input
  put (input', updatePosition startPos spaces)
  pure (not $ ST.null spaces)

xmlNCName :: Parser SText
xmlNCName = do
  (input, startPos) <- get
  case ST.uncons input of
    Nothing -> lift $ throwE $ XMLUnexpectedEoF "NCName"
    Just (c, rest)
      | isNameStartChar c -> do
          put (rest, updatePosition startPos (ST.singleton c))
          cs <- ncNameChars
          let ncname = ST.cons c cs
          unless (isNormalized NFD ncname) $ do
            endPos <- gets snd
            lift $ throwE $ NonNFDName $ PositionRange startPos endPos
          pure ncname
      | otherwise -> lift $ throwE $ XMLBadSyntax "Expected the start of a NCName" startPos
  where
    isNameStartChar c = any (\(lb, ub) -> lb <= c && c <= ub) nameStartRanges
    nameStartRanges =
      [ ('\x41', '\x5A')
      , ('\x5F', '\x5F')
      , ('\x61', '\x7A')
      , ('\xC0', '\xD6')
      , ('\xD8', '\xF6')
      , ('\xF8', '\x2FF')
      , ('\x370', '\x37D')
      , ('\x37F', '\x1FFF')
      , ('\x200C', '\x200D')
      , ('\x2070', '\x218F')
      , ('\x2C00', '\x2FEF')
      , ('\x3001', '\xD7FF')
      , ('\xF900', '\xFDCF')
      , ('\xFDF0', '\xFFFD')
      , ('\x10000', '\xEFFFF')
      ]
    isNCNameChar c = isNameStartChar c || any (\(lb, ub) -> lb <= c && c <= ub) nameNonStartRanges
    nameNonStartRanges =
      [ ('\x2D', '\x2E')
      , ('\x30', '\x39')
      , ('\xB7', '\xB7')
      , ('\x300', '\x36F')
      , ('\x203F', '\x2040')
      ]
    ncNameChars :: Parser SText
    ncNameChars = do
      (input, pos) <- get
      let (chars, rest) = ST.span isNCNameChar input
      put (rest, updatePosition pos chars)
      pure chars

xmlRawQName :: Parser RawQName
xmlRawQName = do
  initial <- xmlNCName
  chooseFrom (pure $ RawQName Nothing initial)
    [ (":", RawQName (Just initial) <$> xmlNCName) ]

data SeenLTRes
  = SeenLTElement UElement
  | SeenLTComment
  | SeenLTDOCTYPE RawQName (Maybe DOCTYPE)
  | SeenLTSlash

data TagStyle = OpenTag | SelfClosing

xmlSeenLT :: Position -> Parser SeenLTRes
xmlSeenLT startPos = chooseFrom (SeenLTElement <$> goElement)
  [ ("!--", do
        void $ charsUntil "--"
        mandatory ">" $ XMLBadSyntax "Expected > to end a comment"
        pure SeenLTComment)
  , ("!DOCTYPE", uncurry SeenLTDOCTYPE <$> xmlSeenDOCTYPE startPos)
  , ("/", pure SeenLTSlash)
  ]
  where
    goElement :: Parser UElement
    goElement = do
      oname <- xmlRawQName
      (attrs, tagTyp) <- goAttrsAndGTOrSelfClose
      body <- case tagTyp of
        SelfClosing -> pure mempty
        OpenTag -> do
          body <- xmlFlowToLTSlash
          endTagPos <- gets snd
          cname <- xmlRawQName
          unless (oname == cname) $
            lift $ throwE $ UnmatchedTags startPos oname endTagPos cname
          void xmlSpaces
          mandatory ">" $ XMLBadSyntax "Expecting '>' to finish a closing tag"
          pure body
      endPos <- gets snd
      pure $ UElement oname attrs body (PositionRange startPos endPos)
    goAttrsAndGTOrSelfClose = do
      spaces <- xmlSpaces
      chooseFrom (goAttr spaces)
        [ ("/>", pure ([], SelfClosing))
        , (">", pure ([], OpenTag))
        ]
    goAttr spaces = do
      attrNameStartPos <- gets snd
      unless spaces $
        lift $ throwE $ XMLBadSyntax "Expected a space before an attribute definition" attrNameStartPos
      name <- xmlRawQName
      attrNameEndPos <- gets snd
      void xmlSpaces
      mandatory "=" $ XMLBadSyntax "Expecting '=', then an attribute value"
      void xmlSpaces
      value <- chooseFrom (lift . throwE . XMLBadSyntax "Expecting an attribute value" =<< gets snd)
        [ ("\"", attributeValue False)
        , ("'", attributeValue True)
        ]
      first ((:) (name, PositionRange attrNameStartPos attrNameEndPos, value)) <$> goAttrsAndGTOrSelfClose
    attributeValue :: Bool -> Parser UContent
    attributeValue isSingleQuoted = do
      valStartPos <- gets snd
      chooseFrom (attributeValueNormalChunk isSingleQuoted)
        [ (if isSingleQuoted then "'" else "\"", pure mempty)
        , ("&", do
              ref <- xmlReference
              rest <- attributeValue isSingleQuoted
              pure $ ref <> rest)
        , ("<", lift . throwE . XMLBadSyntax "'<' in attribute value" =<< gets snd)
        , ("\r", do
            lift $ lift $ tell $ DList.singleton $ AttributeNormalisation valStartPos
            postCRPos <- gets snd
            chooseFrom ((<>) (UContent $ singletonOddA $ Seq.singleton (PositionRange valStartPos postCRPos, " ")) <$> attributeValue isSingleQuoted)
              [ ("\n", do
                  postNLPos <-  gets snd
                  (<>) (UContent $ singletonOddA $ Seq.singleton (PositionRange valStartPos postNLPos, " ")) <$> attributeValue isSingleQuoted)
              ])
        , ("\n", do
            lift $ lift $ tell $ DList.singleton $ AttributeNormalisation valStartPos
            postNLPos <- gets snd
            (<>) (UContent $ singletonOddA $ Seq.singleton (PositionRange valStartPos postNLPos, " ")) <$> attributeValue isSingleQuoted)
        , ("\t", do
            lift $ lift $ tell $ DList.singleton $ AttributeNormalisation valStartPos
            postTabPos <- gets snd
            (<>) (UContent $ singletonOddA $ Seq.singleton (PositionRange valStartPos postTabPos, " ")) <$> attributeValue isSingleQuoted)
        ]
    attributeValueNormalChunk :: Bool -> Parser UContent
    attributeValueNormalChunk isSingleQuoted = do
      (input, chunkStartPos) <- get
      let normalChar = \case
            '&' -> False
            '<' -> False
            '\r' -> False
            '\t' -> False
            '\n' -> False
            '\'' -> not isSingleQuoted
            '"' -> isSingleQuoted
            _ -> True
          (candidates, input') = ST.span normalChar input
          endPos = updatePosition chunkStartPos candidates
      put (input', endPos)
      when (ST.null input') $
        lift $ throwE $ XMLUnexpectedEoF "attribute value."
      case ST.span isXMLChar candidates of
        (valid, rest) -> unless (ST.null rest) $
          lift $ throwE $ XMLNonChar $ updatePosition startPos valid
      let content = UContent $ singletonOddA $ Seq.singleton (PositionRange startPos endPos, candidates)
      rest <- attributeValue isSingleQuoted
      pure $ content <> rest

xmlFlowToLTSlash :: Parser (TwoFingerOddA UElement UContent)
xmlFlowToLTSlash = do
  (input, startPos) <- get
  when (ST.null input) $
    lift $ throwE $ XMLUnexpectedEoF "'</'"
  chooseFrom normalChunk
    [ ("<", do
        next <- xmlSeenLT startPos
        case next of
          SeenLTElement e -> consOddA mempty e <$> xmlFlowToLTSlash
          SeenLTSlash -> pure mempty
          SeenLTComment -> xmlFlowToLTSlash
          SeenLTDOCTYPE _ _ -> lift $ throwE $ StrayDoctype startPos)
    , ("&", do
        content <- xmlReference
        (<>) (singletonOddA content) <$> xmlFlowToLTSlash
        )
    , (">", lift . throwE . XMLStrayGT =<< gets snd)
    , ("\r", do
        postCRPos <- gets snd
        chooseFrom ((<>) (singletonOddA $ UContent $ singletonOddA $ Seq.singleton (PositionRange startPos postCRPos, "\n")) <$> xmlFlowToLTSlash)
          [ ("\n", do
              postNLPos <-  gets snd
              (<>) (singletonOddA $ UContent $ singletonOddA $ Seq.singleton (PositionRange startPos postNLPos, "\n")) <$> xmlFlowToLTSlash)
          ])
    ]
  where
    normalChunk :: Parser (TwoFingerOddA UElement UContent)
    normalChunk = do
      (input, startPos) <- get
      let normalChar = \case
            '&' -> False
            '<' -> False
            '>' -> False
            '\r' -> False
            _ -> True
          (candidates, input') = ST.span normalChar input
          endPos = updatePosition startPos candidates
      put (input', endPos)
      case ST.span isXMLChar candidates of
        (valid, rest) -> unless (ST.null rest) $
          lift $ throwE $ XMLNonChar $ updatePosition startPos valid
      rest <- xmlFlowToLTSlash
      pure $ singletonOddA (UContent $ singletonOddA $ Seq.singleton (PositionRange startPos endPos, candidates)) <> rest

xmlReference :: Parser UContent
xmlReference = do
  res <- chooseFrom refEnt [("#", refChar)]
  mandatory ";" $ XMLBadSyntax "Expecting ';' to terminate an entity or a character reference"
  pure res
  where
    refEnt = do
      start <- gets snd
      name <- xmlNCName
      end <- gets snd
      pure $ UContent $ unitOddA (PositionRange start end, name)
    refChar = do
      start <- gets snd
      --Notes on magic numbers: max Char is 0x10FFFF = 114111. 7 digits are 24 bits, so Int is safe.
      n <- chooseFrom (decimalRef 0 7) [("x", hexadecimalRef 0 6)]
      end <- gets snd
      case toEnumMay n of
        Nothing -> lift $ throwE $ BadCharRef (PositionRange start end)
        Just c -> do
          unless (isXMLChar c) $
            lift $ throwE $ BadCharRef (PositionRange start end)
          pure $ UContent $ singletonOddA $ Seq.singleton (PositionRange start end, ST.singleton c)
    --TODO: wouldn't it be better to use Nat for the parameter??
    decimalRef :: Int -> Natural -> Parser Int
    decimalRef acc 0 = pure acc
    decimalRef acc n = chooseFrom (pure acc)
      [ ("0", decimalRef (acc * 10 + 0) (n - 1))
      , ("1", decimalRef (acc * 10 + 1) (n - 1))
      , ("2", decimalRef (acc * 10 + 2) (n - 1))
      , ("3", decimalRef (acc * 10 + 3) (n - 1))
      , ("4", decimalRef (acc * 10 + 4) (n - 1))
      , ("5", decimalRef (acc * 10 + 5) (n - 1))
      , ("6", decimalRef (acc * 10 + 6) (n - 1))
      , ("7", decimalRef (acc * 10 + 7) (n - 1))
      , ("8", decimalRef (acc * 10 + 8) (n - 1))
      , ("9", decimalRef (acc * 10 + 9) (n - 1))
      ]
    hexadecimalRef :: Int -> Natural -> Parser Int
    hexadecimalRef acc 0 = pure acc
    hexadecimalRef acc n = chooseFrom (pure acc)
      [ ("0", hexadecimalRef (acc * 16 +  0) (n - 1))
      , ("1", hexadecimalRef (acc * 16 +  1) (n - 1))
      , ("2", hexadecimalRef (acc * 16 +  2) (n - 1))
      , ("3", hexadecimalRef (acc * 16 +  3) (n - 1))
      , ("4", hexadecimalRef (acc * 16 +  4) (n - 1))
      , ("5", hexadecimalRef (acc * 16 +  5) (n - 1))
      , ("6", hexadecimalRef (acc * 16 +  6) (n - 1))
      , ("7", hexadecimalRef (acc * 16 +  7) (n - 1))
      , ("8", hexadecimalRef (acc * 16 +  8) (n - 1))
      , ("9", hexadecimalRef (acc * 16 +  9) (n - 1))
      , ("A", hexadecimalRef (acc * 16 + 10) (n - 1))
      , ("B", hexadecimalRef (acc * 16 + 11) (n - 1))
      , ("C", hexadecimalRef (acc * 16 + 12) (n - 1))
      , ("D", hexadecimalRef (acc * 16 + 13) (n - 1))
      , ("E", hexadecimalRef (acc * 16 + 14) (n - 1))
      , ("F", hexadecimalRef (acc * 16 + 15) (n - 1))
      , ("a", hexadecimalRef (acc * 16 + 10) (n - 1))
      , ("b", hexadecimalRef (acc * 16 + 11) (n - 1))
      , ("c", hexadecimalRef (acc * 16 + 12) (n - 1))
      , ("d", hexadecimalRef (acc * 16 + 13) (n - 1))
      , ("e", hexadecimalRef (acc * 16 + 14) (n - 1))
      , ("f", hexadecimalRef (acc * 16 + 15) (n - 1))
      ]

data DOCTYPE
  = DOCTYPESystem SText
  | DOCTYPEPublic SText SText
  deriving (Show, Generic)

instance NFData DOCTYPE

getSystemId :: DOCTYPE -> SText
getSystemId (DOCTYPESystem sys) = sys
getSystemId (DOCTYPEPublic _ sys) = sys

xmlSeenDOCTYPE :: Position -> Parser (RawQName, Maybe DOCTYPE)
xmlSeenDOCTYPE startPos = do
  leadingSpaces <- xmlSpaces
  unless leadingSpaces $
    lift $ throwE $ XMLBadSyntax "Expected a space following 'DOCTYPE'" startPos
  rootName <- xmlRawQName
  succSpaces <- xmlSpaces
  res <- chooseFrom (pure Nothing)
    [ ("SYSTEM", do
         unless succSpaces $ do
           spacePos <- gets snd
           lift $ throwE $ XMLBadSyntax "Expected a space between the root element name and 'SYSTEM" spacePos
         fmap Just $ DOCTYPESystem <$> systemLiteral)
    , ("PUBLIC", do
         unless succSpaces $
           lift $ throwE $ XMLBadSyntax "Expected a space between the root element name and 'PUBLIC'" startPos
         sepSpaces <- xmlSpaces
         unless sepSpaces $ do
           spacePos <- gets snd
           lift $ throwE $ XMLBadSyntax "Expected a space before the public literal" spacePos
         fmap Just $ DOCTYPEPublic <$> publicLiteral <*> systemLiteral)
    ]
  void xmlSpaces
  mandatory ">" $ XMLBadSyntax "Expected '>' to close a DOCTYPE"
  pure (rootName, res)
  where
    systemLiteral :: Parser SText
    systemLiteral = do
      sepSpaces <- xmlSpaces
      unless sepSpaces $ do
        spacePos <- gets snd
        lift $ throwE $ XMLBadSyntax "Expected a space before the system literal" spacePos
      chooseFrom (lift . throwE . XMLBadSyntax "Expected a system literal" =<< gets snd)
        [ ("\"", charsUntil "\"")
        , ("'",  charsUntil "'")
        ]
    publicLiteral = do
      mandatory "\"" $ XMLBadSyntax "Expected a public literal"
      ident <- pubIdChars
      mandatory "\"" $ XMLBadSyntax "Expected '\"' to terminate a public literal"
      pure ident
    pubIdCharRanges =
      [ ('\n', '\n')
      , ('\xD', '\xD')
      , (' ', '!')
      , ('#', '%')
      , ('\'', ';')
      , ('=', 'Z')
      , ('_', '_')
      , ('a', 'z')
      ]
    pubIdChars :: Parser SText
    pubIdChars = do
      (input, pos) <- get
      let (cs, input') = ST.span (\c -> any (\(lb, ub) -> lb <= c && c <= ub) pubIdCharRanges) input
      put (input', updatePosition pos cs)
      pure cs

chooseFrom ::  Parser r -> [(SText, Parser r)] -> Parser r
chooseFrom def opts = do
  (input, pos) <- get
  case find (flip ST.isPrefixOf input . fst) opts of
    Nothing -> def
    Just (prefix, fn) -> do
      let input' = ST.drop (ST.length prefix) input
          pos' = updatePosition pos prefix
      put (input', pos')
      fn

mandatory :: SText -> (Position -> XMLError) -> Parser ()
mandatory prefix err =
  chooseFrom (lift . throwE . err =<< gets snd)
    [(prefix, pure ())]

charsUntil :: SText -> Parser SText
charsUntil needle = do
  (input, pos) <- get
  --TODO: this interface is obviously clunky; maybe should ask upstream for a nicer one?
  let (cs, rest) = ST.breakOn needle input
      input' = ST.drop (ST.length needle) rest
  when (ST.null rest) $
    lift $ throwE $ XMLUnexpectedEoF $ "'" <> needle <> "'"
  case ST.span isXMLChar cs of
    (success, nonChars) -> unless (ST.null nonChars) $
      lift $ throwE $ XMLNonChar $ updatePosition pos success
  put (input', updatePosition pos $ cs <> needle)
  pure cs

isXMLChar :: Char -> Bool
isXMLChar c = any (\(lb, ub) -> lb <= c && c <= ub) ranges
  where
    ranges =
      [ ('\x9', '\x9')
      , ('\xA', '\xA')
      , ('\xD', '\xD')
      , ('\x20', '\xD7FF')
      , ('\xE000', '\xFFFD')
      , ('\x10000', '\x10FFFF')
      ]
