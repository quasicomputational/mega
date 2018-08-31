{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
-- |
-- Stability: unstable
--
-- This is an __internal module__ and __not subject to the PVP__. It
-- may receive arbitrary changes at any time and between any two
-- releases. Import from "Q4C12.XML" instead, unless you really
-- need the gory details, and, in that case, you must depend on the
-- __exact__ version of this package. (If you do need them, please
-- file a bug so that, hopefully, your use-case can be accomplished
-- through the public interface.)
module Q4C12.XML.Internal where

import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State (get, gets, put, modify)
import Control.Monad.Trans.Writer.CPS (tell)
import qualified Data.DList as DList
import qualified Data.DList.NonEmpty as NEDList
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.String (IsString (fromString))
import qualified Data.Text as ST
import Data.Text.ICU.Normalize (isNormalized, NormalizationMode (NFD))
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import Q4C12.MapPend (MapPend, getMapPend)
import qualified Q4C12.MapPend as MapPend
import Q4C12.Position (Position, startPosition, PositionRange (PositionRange), updatePosition, position, positionRange)
import Q4C12.TwoFinger (singletonOddA, unitOddA, consOddA)
import Safe (toEnumMay)

data QName = QName !(Maybe SText) !SText
  deriving (Show, Eq, Ord, Generic, Data)

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

newtype Comment pos = Comment { getComment :: Seq (pos, LText) }
  deriving (Show, Generic, Data)

instance (NFData pos) => NFData (Comment pos)

-- Comments (of type cmt) and text.
newtype Content cmt pos = Content { getContent :: TwoFingerOddA cmt (Seq (pos, LText)) }
  deriving (Show, Generic, Data)

instance (NFData cmt, NFData pos) => NFData (Content cmt pos)

instance Bifunctor Content where
  bimap f g (Content tree) =
    Content $ bimap f (fmap $ first g) tree

instance Functor (Content cmt) where
  fmap = bimap id

instance Semigroup (Content cmt pos) where
  Content a <> Content b = Content $ a <> b

instance Monoid (Content cmd pos) where
  mempty = Content mempty
  mappend = (<>)

instance (pos ~ ()) => IsString (Content cmt pos) where
  fromString = contentText . fromString

-- | Note: treats comment-only nodes as null.
contentNull :: Content cmt pos -> Bool
contentNull = all (all (LT.null . snd)) . getContent

contentTextPos :: pos -> LText -> Content cmt pos
contentTextPos pos t = Content $ singletonOddA $ pure (pos, t)

-- This is a monoid homomorphism:
--   contentText (a <> b) = contentText a <> contentText b
-- (up to internal implementation details)
contentText :: LText -> Content cmt ()
contentText = contentTextPos ()

contentComment :: cmt -> Content cmt pos
contentComment = Content . unitOddA

newtype Markup cmt pos = Markup { getMarkup :: TwoFingerOddA (Element cmt pos) (Content cmt pos)  }
  deriving (Show, Generic, Data)

instance (NFData cmt, NFData pos) => NFData (Markup cmt pos)

instance Bifunctor Markup where
  bimap f g (Markup ilv) =
    Markup $ bimap (bimap f g) (bimap f g) ilv

instance Functor (Markup cmt) where
  fmap = bimap id

instance Semigroup (Markup cmt pos) where
  Markup a <> Markup b = Markup $ a <> b

instance Monoid (Markup cmt pos) where
  mempty = Markup mempty
  mappend = (<>)

instance (pos ~ ()) => IsString (Markup cmt pos) where
  fromString = markupText . fromString

-- | Note: treats comment-only nodes as null.
markupNull :: Markup cmt pos -> Bool
markupNull (Markup tree) = biall (const False) contentNull tree

--TODO: shouldn't attr values be strict?
data Element cmt pos = Element
  { _ename :: QName
  , _eattrs :: Map QName (pos, Seq (pos, LText))
  , _ebody :: Markup cmt pos
  , elementPosition :: pos
  }
  deriving (Show, Functor, Generic, Data)

instance Bifunctor Element where
  bimap f g (Element name attrs body pos) =
    Element name (fmap (bimap g (fmap (first g))) attrs) (bimap f g body) (g pos)

instance (NFData cmt, NFData pos) => NFData (Element cmt pos)

eattrs :: Lens' (Element cmt pos) (Map QName (pos, Seq (pos, LText)))
eattrs f (Element n attrs body pos) = f attrs <&>
  \attrs' -> Element n attrs' body pos

markupElement :: Element cmt pos -> Markup cmt pos
markupElement = Markup . unitOddA

markupTextPos :: pos -> LText -> Markup cmt pos
markupTextPos pos t = Markup $ singletonOddA $ contentTextPos pos t

markupText :: LText -> Markup cmt ()
markupText = markupTextPos ()

markupBuilder :: TBuilder -> Markup cmt ()
markupBuilder = markupText . LTB.toLazyText

markupSText :: SText -> Markup cmt ()
markupSText = markupSTextPos ()

markupSTextPos :: pos -> SText -> Markup cmt pos
markupSTextPos pos = markupTextPos pos . LT.fromStrict

element :: QName -> Markup cmt () -> Element cmt ()
element qname markup = Element qname mempty markup ()

--mappends on the left, to take priority
addAttrs :: Map QName LText -> Element cmt () -> Element cmt ()
addAttrs attrs = eattrs <>~ (attrs <&> \a -> ((), Seq.singleton ((), a)))

at :: (Ord k, Functor f) => k -> (Maybe v -> f (Maybe v)) -> Map k v -> f (Map k v)
at = flip Map.alterF

addAttr :: QName -> LText -> Element cmt () -> Element cmt ()
addAttr qname val = set (eattrs . at qname) (Just ((), Seq.singleton ((), val)))

addAttrS :: QName -> SText -> Element cmt () -> Element cmt ()
addAttrS qname = addAttr qname . LT.fromStrict

addUAttrS :: SText -> SText -> Element cmt () -> Element cmt ()
addUAttrS name = addUAttr name . LT.fromStrict

addUAttr :: SText -> LText -> Element cmt () -> Element cmt ()
addUAttr = addAttr . uname

rngelem :: SText -> Markup cmt () -> Element cmt ()
rngelem = element . rngname

helem :: SText -> Markup cmt () -> Element cmt ()
helem = element . hname

-- | Note: this assumes that the comments are well-formed: they must
-- not contain a final '-' or '--' anywhere. Also note that
-- end-of-line conversion will mean that the distinction between CRLF,
-- CR and LF will be lost upon re-parsing.
renderXML :: Element (Comment pos) pos' -> TBuilder
renderXML = execWriter . renderElement

renderElement :: Element (Comment pos) pos' -> Writer TBuilder ()
renderElement (Element name attrs markup _) = flip evalStateT 1 $ do
  lift $ tell "<"
  cur <- get
  lift $ renderQName cur name
  renderNamespaceFor name
  for_ (Map.toAscList attrs) $ \(attrName, (_, chunks)) -> do
    lift $ tell " "
    n <- get
    lift $ renderQName n attrName
    lift $ tell "=\""
    lift $ traverse_ (renderAttrText . snd) chunks
    lift $ tell "\""
    renderNamespaceFor attrName
  lift $ tell ">"
  lift $ renderMarkup markup
  lift $ tell "</"
  lift $ renderQName cur name
  lift $ tell ">"
  where
    renderQName :: Word -> QName -> Writer TBuilder ()
    renderQName _ (QName Nothing local) =
      tell $ LTB.fromText local
    renderQName _ (QName (Just ns) local) | ns == xmlNS =
      tell $ "xml:" <> LTB.fromText local
    renderQName cur (QName (Just _) local) =
      tell $ "n" <> LTBI.decimal cur <> ":" <> LTB.fromText local
    renderNamespaceFor :: QName -> StateT Word (Writer TBuilder) ()
    renderNamespaceFor (QName Nothing _) = pure ()
    renderNamespaceFor (QName (Just ns) _) | ns == xmlNS =
      pure ()
    renderNamespaceFor (QName (Just ns) _) = do
      cur <- get
      lift $ tell $ " xmlns:n" <> LTBI.decimal cur <> "=\""
      lift $ renderAttrText $ LT.fromStrict ns
      lift $ tell "\""
      modify (+ 1)

renderMarkup :: Markup (Comment pos) pos' -> Writer TBuilder ()
renderMarkup (Markup tree) =
  bitraverse_ renderElement renderContent tree

renderContent :: Content (Comment pos) pos' -> Writer TBuilder ()
renderContent (Content tree) =
  bitraverse_ renderComment (traverse (renderBodyText . snd)) tree

renderComment :: Comment pos -> Writer TBuilder ()
renderComment (Comment parts) = do
  tell "<!--"
  for_ parts $ \ (_, cmt) ->
    tell $ LTB.fromLazyText cmt
  tell "-->"

renderBodyText :: LText -> Writer TBuilder ()
renderBodyText = tell . LTB.fromText . ST.replace "\r" "&#xD;" . ST.replace "<" "&lt;" . ST.replace ">" "&gt;" . ST.replace "&" "&amp;" . LT.toStrict

renderAttrText :: LText -> Writer TBuilder ()
renderAttrText = tell . LTB.fromText . ST.replace "\n" "&#xA;" . ST.replace "\r" "&#xD;" . ST.replace "\t" "&#x9;" . ST.replace "\"" "&quot;" . ST.replace "<" "&lt;" . ST.replace ">" "&gt;" . ST.replace "&" "&amp;" . LT.toStrict

--(prefix, local); this is used before we have mapped prefices to namespaces.
data RawQName = RawQName (Maybe SText) SText
  deriving (Eq, Ord, Show, Generic)

instance NFData RawQName

rawQName :: RawQName -> TBuilder
rawQName = \case
  RawQName Nothing local ->
    LTB.fromText local
  RawQName (Just pref) local -> fold
    [ LTB.fromText pref, ":", LTB.fromText local ]

--TODO: it'd be nice to be able to mark individual entities as deprecated, wouldn't it?
--TODO: factor the various 'stray X at Y' errors together?
data XMLError
  = UnmatchedTags Position RawQName Position RawQName
  | PreRootText Position
  | TrailingText Position
  | TrailingStartTag Position Position
  | TrailingEndTag Position Position
  | TrailingRef Position
  | NoRoot
  | PreRootEndTag Position
  | PreRootRef Position
  | XMLBadSyntax SText Position
  | XMLUnexpectedEoF SText
  | XMLNonChar Position
  | BadCharRef PositionRange
  | NonNFDName PositionRange
  | XMLStrayGT Position
  | ResolveError (NonEmpty ResolveError)
  deriving (Show, Generic, Eq, Ord)

instance NFData XMLError

data ResolveError
  = UnknownEntity PositionRange SText
  | UnboundNamespace PositionRange SText
  | DuplicateAttrs PositionRange (NonEmpty PositionRange)
  | DuplicateNamespacePrefixDecls SText PositionRange (NonEmpty PositionRange)
  | DuplicateDefaultNamespaceDecls PositionRange (NonEmpty PositionRange)
  | NonNFDNamespace PositionRange
  deriving (Show, Generic, Eq, Ord)

instance NFData ResolveError

data XMLWarning
  = AttributeNormalisation Position
  deriving (Show, Generic, Eq, Ord)

instance NFData XMLWarning

displayResolveError :: ResolveError -> TBuilder
displayResolveError (UnknownEntity pos name) = fold
  --TODO: escape the entity name??
  [ "Error: Unknown entity '", LTB.fromText name, "' at ", positionRange pos, "." ]
displayResolveError (UnboundNamespace pos pref) = fold
  [ "Error: Unknown namespace prefix '", LTB.fromText pref, "' at ", positionRange pos, "." ]
displayResolveError (DuplicateAttrs r0 rs) = fold
  [ "Error: Duplicate attributes at ", intercalateMap0 ", " positionRange rs, " and ", positionRange r0, "." ]
displayResolveError (DuplicateNamespacePrefixDecls pref r0 rs) = fold
  [ "Error: Duplicate definitions for prefix '", LTB.fromText pref, "' at ", intercalateMap0 ", " positionRange rs, " and ", positionRange r0, "." ]
displayResolveError (DuplicateDefaultNamespaceDecls r0 rs) = fold
  [ "Error: Duplicate default namespace definitions at ", intercalateMap0 ", " positionRange rs, " and ", positionRange r0, "." ]
displayResolveError (NonNFDNamespace r) = fold
  [ "Error: Namespace is not in NFD at ", positionRange r, "." ]

displayError :: XMLError -> TBuilder
displayError (UnmatchedTags opos open cpos close) = fold
  [ "Error: Misnested tags: Open tag is '", rawQName open, "' at ", position opos
  , ", but close tag is '", rawQName close, "' at ", position cpos, "."
  ]
displayError (PreRootText pos) = fold
  [ "Error: Unexpected text at ", position pos, " before the root element." ]
displayError (TrailingText pos) = fold
  [ "Error: Unexpected text at ", position pos, " after the root element." ]
displayError (TrailingEndTag rootClosePos tagPos) = fold
  [ "Error: Trailing closing tag at ", position tagPos
  , "; the root element was closed at ", position rootClosePos, "."
  ]
displayError (TrailingStartTag rootClosePos tagPos) = fold
  [ "Error: Trailing opening tag at ", position tagPos
  , "; the root element was closed at ", position rootClosePos, "."
  ]
displayError (TrailingRef pos) = fold
  [ "Error: Reference at ", position pos, ", after the root element." ]
displayError NoRoot = "Error: Missing root element."
displayError (PreRootRef pos) = fold
  [ "Error: Reference at ", position pos, ", before the root element." ]
displayError (PreRootEndTag pos) = fold
  [ "Error: End tag at ", position pos, ", before the root element." ]
displayError (ResolveError errs) =
  intercalateMap0 "\n" displayResolveError errs
displayError (XMLUnexpectedEoF instead) = fold
  [ "Error: End of input seen, instead of expected ", LTB.fromText instead, "." ]
displayError (XMLBadSyntax msg pos) = fold
  [ "Error: Bad syntax: ", LTB.fromText msg, " at ", position pos, "." ]
displayError (XMLNonChar pos) = fold
  [ "Error: Non-XML character at ", position pos, "." ]
displayError (BadCharRef pos) = fold
  [ "Error: Character reference is not an XML character at ", positionRange pos, "." ]
displayError (XMLStrayGT pos) = fold
  [ "Error: Stray '>' at ", position pos, "." ]
displayError (NonNFDName pos) = fold
  [ "Error: Non-NFD attribute or element name at ", positionRange pos, "." ]

displayWarnings :: (Foldable f) => f XMLWarning -> TBuilder
displayWarnings = foldMap (\warn -> displayWarning warn <> "\n")

--TODO: a -Werror option, which would mean prefixing this with 'error', not 'warning'.
--TODO: escape the name?
displayWarning :: XMLWarning -> TBuilder
displayWarning (AttributeNormalisation pos) = fold
  [ "Warning: Performing attribute normalisation at ", position pos, "." ]

data UElement = UElement
  RawQName
  [(RawQName, PositionRange, UText)]
  UMarkup
  PositionRange

newtype UMarkup =
  UMarkup { getUMarkup :: TwoFingerOddA UElement UContent }

instance Semigroup UMarkup where
  UMarkup a <> UMarkup b = UMarkup (a <> b)

instance Monoid UMarkup where
  mempty = UMarkup mempty
  mappend = (<>)

umarkupText :: PositionRange -> SText -> UMarkup
umarkupText pos t = UMarkup $ singletonOddA $ ucontentText pos t

newtype UContent =
  UContent (TwoFingerOddA (Comment PositionRange) UText)

instance Semigroup UContent where
  UContent a <> UContent b = UContent (a <> b)

instance Monoid UContent where
  mempty = UContent mempty
  mappend = (<>)

ucontentText :: PositionRange -> SText -> UContent
ucontentText pos t = UContent $ singletonOddA $ utext pos t

ucontentComment :: Comment PositionRange -> UContent
ucontentComment = UContent . unitOddA

-- Straightforwards text interspersed with unresolved entity references.
newtype UText =
  UText (TwoFingerOddA (PositionRange, SText) (Seq (PositionRange, SText)))

instance Semigroup UText where
  UText a <> UText b = UText (a <> b)

instance Monoid UText where
  mempty = UText mempty
  mappend = (<>)

utext :: PositionRange -> SText -> UText
utext pos t = UText $ singletonOddA $ Seq.singleton (pos, t)

resolveText
  :: UText
  -> Validation (NonEmptyDList ResolveError) (Seq (PositionRange, LText))
resolveText (UText parts) = bifoldMapM resolveEntity (pure . fmap (fmap LT.fromStrict)) parts
  where
    resolveEntity :: (PositionRange, SText) -> Validation (NonEmptyDList ResolveError) (Seq (PositionRange, LText))
    resolveEntity (pos, "amp") = pure $ Seq.singleton (pos, "&")
    resolveEntity (pos, "gt") = pure $ Seq.singleton (pos, ">")
    resolveEntity (pos, "lt") = pure $ Seq.singleton (pos, "<")
    resolveEntity (pos, "quot") = pure $ Seq.singleton (pos, "\"")
    resolveEntity (pos, "apos") = pure $ Seq.singleton (pos, "'")
    resolveEntity (pos, name) = failure $ NEDList.singleton $ UnknownEntity pos name

resolveContent
  :: UContent
  -> Validation (NonEmptyDList ResolveError) (Content (Comment PositionRange) PositionRange)
resolveContent (UContent tree) =
  Content <$> traverse resolveText tree

resolveMarkup
  :: Maybe SText
  -> Map SText SText
  -> UMarkup
  -> Validation (NonEmptyDList ResolveError) (Markup (Comment PositionRange) PositionRange)
resolveMarkup defaultNamespace namespaces (UMarkup tree) =
  Markup <$> bitraverse (resolveElement defaultNamespace namespaces) resolveContent tree

data ClassifiedAttrs = ClassifiedAttrs
  { _classifiedDefaultNamespaces :: DList (PositionRange, SText)
  , _classifiedBoundNamespaces :: MapPend SText (NonEmptyDList (PositionRange, SText))
  , _classifiedAttrs :: DList (PositionRange, RawQName, Seq (PositionRange, LText))
  }

instance Semigroup ClassifiedAttrs where
  ClassifiedAttrs ds nss as <> ClassifiedAttrs ds' nss' as' =
    ClassifiedAttrs (ds <> ds') (nss <> nss') (as <> as')

instance Monoid ClassifiedAttrs where
  mempty = ClassifiedAttrs mempty mempty mempty
  mappend = (<>)

classifyDefaultNamespace
  :: PositionRange
  -> SText
  -> ClassifiedAttrs
classifyDefaultNamespace pos ns =
  ClassifiedAttrs (DList.singleton (pos, ns)) mempty mempty

classifyNamespaceBinding
  :: SText
  -> PositionRange
  -> SText
  -> ClassifiedAttrs
classifyNamespaceBinding local pos ns =
  ClassifiedAttrs mempty (MapPend.singleton local $ NEDList.singleton (pos, ns)) mempty

classifyAttr
  :: PositionRange
  -> RawQName
  -> Seq (PositionRange, LText)
  -> ClassifiedAttrs
classifyAttr pos name val =
  ClassifiedAttrs mempty mempty (DList.singleton (pos, name, val))

resolveElement
  :: Maybe SText
  -> Map SText SText
  -> UElement
  -> Validation (NonEmptyDList ResolveError) (Element (Comment PositionRange) PositionRange)
resolveElement defaultNamespace namespaces (UElement rawName rawAttrs rawBody pos) = exceptToValidation $ do
  --TODO: can do more of this in parallel and report more errors at once than we currently are.
  -- Attribute checking and namespace definition extraction.
  -- First, we go over the attributes and split them into three categories: default namespace declarations, named namespace declarations, and content-bearing attributes.
  rawAttrs' <- validationToExcept $ for rawAttrs $ \(rawAttrName, attrPos, rawText) ->
    (,,) rawAttrName attrPos <$> resolveText rawText
  let ClassifiedAttrs defaultNamespaces collatedNamespaceBindings otherAttrs = flip foldMap rawAttrs' $ \(RawQName mpref local, nameRange, value) ->
        case mpref of
          Nothing | local == "xmlns" ->
            classifyDefaultNamespace nameRange (LT.toStrict $ foldMap snd value)
          Just "xmlns" ->
            classifyNamespaceBinding local nameRange (LT.toStrict $ foldMap snd value)
          _ ->
            classifyAttr nameRange (RawQName mpref local) value
  -- Go over our three lists of attribute types and verify appropriate uniqueness and well-formedness for each of them.
  defaultNamespace' <- validationToExcept $ case toList defaultNamespaces of
    [] -> pure defaultNamespace
    (_, "") : [] -> pure Nothing
    (_, ns) : [] | isNormalized NFD ns -> pure (Just ns)
    (r0, _) : [] -> failure $ NEDList.singleton $ NonNFDNamespace r0
    (r0, _) : (r1, _) : rest -> failure $ NEDList.singleton $ DuplicateDefaultNamespaceDecls r0 (r1 :| fmap fst (toList rest))
  namespaceBindings <- validationToExcept $
    flip Map.traverseWithKey (getMapPend collatedNamespaceBindings) $ \pref binds ->
      case toNonEmpty binds of
        (_, ns) :| [] | isNormalized NFD ns -> pure ns
        (r, _) :| [] -> failure $ NEDList.singleton $ NonNFDNamespace r
        (r0, _) :| ((r1, _) : rest) ->
          failure $ NEDList.singleton $ DuplicateNamespacePrefixDecls pref r0 (r1 :| fmap fst rest)
  let namespaces' = namespaceBindings <> namespaces --Note: new on the left, to take precedence.
  qname <- validationToExcept $ case rawName of
    RawQName Nothing local -> pure $ QName defaultNamespace' local
    RawQName (Just pref) local -> case Map.lookup pref namespaces' of
      Nothing -> failure $ NEDList.singleton $ UnboundNamespace pos pref
      Just ns -> pure $ QName (Just ns) local
  attrSets <- validationToExcept $ fmap getMapPend $
    flip foldMapM otherAttrs $ \(r, RawQName mpref local, value) ->
      case mpref of
        Nothing -> pure $ MapPend.singleton (QName Nothing local) $ pure (r, value)
        Just pref -> case Map.lookup pref namespaces' of
          Nothing -> failure $ NEDList.singleton $ UnboundNamespace r pref
          Just ns -> pure $ MapPend.singleton (QName (Just ns) local) $ pure (r, value)
  attrs <- validationToExcept $ for attrSets $ \case
    (r, value) :| [] -> pure (r, value)
    (r0, _) :| ((r1, _) : rest) -> failure $ NEDList.singleton $ DuplicateAttrs r0 (r1 :| fmap fst rest)
  -- Finally, recurse into our children.
  body <- validationToExcept $
    resolveMarkup defaultNamespace' namespaces' rawBody
  pure $ Element qname attrs body pos

data Parsed = Parsed
  { _preRootComments :: Seq (Comment PositionRange)
  , _rootElement :: Element (Comment PositionRange) PositionRange
  , _postRootComments :: Seq (Comment PositionRange)
  }
  deriving (Show)

preRootComments :: Lens' Parsed (Seq (Comment PositionRange))
preRootComments k p =
  (\ x -> p { _preRootComments = x }) <$> k (_preRootComments p)

rootElement :: Lens' Parsed (Element (Comment PositionRange) PositionRange)
rootElement k p =
  (\ x -> p { _rootElement = x }) <$> k (_rootElement p)

postRootComments :: Lens' Parsed (Seq (Comment PositionRange))
postRootComments k p =
  (\ x -> p { _postRootComments = x }) <$> k (_postRootComments p)

stripCommentsContent :: Content cmt pos -> Content cmt' pos
stripCommentsContent (Content tree) =
  Content $ singletonOddA $ fold tree

stripCommentsMarkup :: Markup cmt pos -> Markup cmt' pos
stripCommentsMarkup (Markup tree) =
  Markup (bimap stripComments stripCommentsContent tree)

stripComments :: Element cmt pos -> Element cmt' pos
stripComments (Element qn attrs markup pos) =
  Element qn attrs (stripCommentsMarkup markup) pos

--TODO: should this accept a filename to report errors from?
parseXML
  :: SText
  -> (Either XMLError (Element cmt PositionRange), [XMLWarning])
parseXML input =
  first (fmap $ stripComments . view rootElement) $
    parseXMLCommented input

parseXMLCommented
  :: SText
  -> (Either XMLError Parsed, [XMLWarning])
parseXMLCommented input = fmap toList $ runWriter $ runExceptT $ evalStateT (toplevel mempty) (input, startPosition)

toplevel
  :: Seq (Comment PositionRange)
  -> Parser Parsed
toplevel preCmts = do
  void xmlSpaces
  (input, pos) <- get
  when (ST.null input) $
    lift $ throwE NoRoot
  chooseFrom (lift $ throwE $ PreRootText pos)
    [ ("&", lift $ throwE $ PreRootRef pos)
    , ("<", do
        res <- xmlSeenLT pos
        case res of
          SeenLTElement e -> do
            endPos <- gets snd
            postCmts <- finish endPos mempty
            e' <- lift $ withExceptT (ResolveError . toNonEmpty) $ validationToExcept $
              resolveElement Nothing defaultNamespaces e
            pure $ Parsed preCmts e' postCmts
          SeenLTComment cmt ->
            toplevel (preCmts Seq.|> cmt)
          SeenLTSlash -> lift $ throwE $ PreRootEndTag pos)
    ]
  where
    defaultNamespaces :: Map SText SText
    defaultNamespaces = Map.fromList
      [ ("xml", xmlNS)
      ]
    finish rootPos postCmts = do
      void xmlSpaces
      (input, pos) <- get
      if ST.null input
        then pure postCmts
        else chooseFrom (lift $ throwE $ TrailingText pos)
          [ ("&", lift $ throwE $ TrailingRef pos)
          , ("<", do
              res <- xmlSeenLT pos
              case res of
                SeenLTElement _ -> lift $ throwE $ TrailingStartTag rootPos pos
                SeenLTComment cmt -> finish rootPos (postCmts Seq.|> cmt)
                SeenLTSlash -> lift $ throwE $ TrailingEndTag rootPos pos)
          ]

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
  | SeenLTComment (Comment PositionRange)
  | SeenLTSlash

data TagStyle = OpenTag | SelfClosing

xmlSeenLT :: Position -> Parser SeenLTRes
xmlSeenLT startPos = chooseFrom (SeenLTElement <$> goElement)
  [ ("!--", SeenLTComment . Comment <$> goComment)
  , ("/", pure SeenLTSlash)
  ]
  where
    goComment :: Parser (Seq (PositionRange, LText))
    goComment = do
      valStartPos <- gets snd
      chooseFrom commentNormalChunk
        [ ("-", do
            postDashPos <- gets snd
            chooseFrom ((Seq.<|) (PositionRange valStartPos postDashPos, "-") <$> goComment)
              [ ("-", do
                  mandatory ">" $ XMLBadSyntax "Expected > to end a comment"
                  pure mempty)
              ])
        --TODO issue warnings for comment lossiness?
        , ("\r", do
            postCRPos <- gets snd
            chooseFrom ((Seq.<|) (PositionRange valStartPos postCRPos, "\n") <$> goComment)
              [ ("\n", do
                  postNLPos <-  gets snd
                  (Seq.<|) (PositionRange valStartPos postNLPos, "\n") <$> goComment)
              ])
        , ("\n", do
            postNLPos <- gets snd
            (Seq.<|) (PositionRange valStartPos postNLPos, "\n") <$> goComment)
        ]

    commentNormalChunk :: Parser (Seq (PositionRange, LText))
    commentNormalChunk = do
      (input, chunkStartPos) <- get
      let normalChar = \case
            '-' -> False
            '\r' -> False
            '\n' -> False
            _ -> True
          (candidates, input') = ST.span normalChar input
          endPos = updatePosition chunkStartPos candidates
      put (input', endPos)
      when (ST.null input') $
        lift $ throwE $ XMLUnexpectedEoF "end of comment"
      case ST.span isXMLChar candidates of
        (valid, rest) -> unless (ST.null rest) $
          lift $ throwE $ XMLNonChar $ updatePosition startPos valid
      rest <- goComment
      pure $ (PositionRange startPos endPos, LT.fromStrict candidates) Seq.<| rest

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

    attributeValue :: Bool -> Parser UText
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
            chooseFrom ((<>) (utext (PositionRange valStartPos postCRPos) " ") <$> attributeValue isSingleQuoted)
              [ ("\n", do
                  postNLPos <-  gets snd
                  (<>) (utext (PositionRange valStartPos postNLPos) " ") <$> attributeValue isSingleQuoted)
              ])
        , ("\n", do
            lift $ lift $ tell $ DList.singleton $ AttributeNormalisation valStartPos
            postNLPos <- gets snd
            (<>) (utext (PositionRange valStartPos postNLPos) " ") <$> attributeValue isSingleQuoted)
        , ("\t", do
            lift $ lift $ tell $ DList.singleton $ AttributeNormalisation valStartPos
            postTabPos <- gets snd
            (<>) (utext (PositionRange valStartPos postTabPos) " ") <$> attributeValue isSingleQuoted)
        ]

    attributeValueNormalChunk :: Bool -> Parser UText
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
      let content = utext (PositionRange startPos endPos) candidates
      rest <- attributeValue isSingleQuoted
      pure $ content <> rest

xmlFlowToLTSlash :: Parser UMarkup
xmlFlowToLTSlash = do
  (input, startPos) <- get
  when (ST.null input) $
    lift $ throwE $ XMLUnexpectedEoF "'</'"
  chooseFrom normalChunk
    [ ("<", do
        next <- xmlSeenLT startPos
        case next of
          SeenLTElement e -> do
            UMarkup tree <- xmlFlowToLTSlash
            pure $ UMarkup $ consOddA mempty e tree
          SeenLTSlash -> pure mempty
          SeenLTComment cmt -> do
            UMarkup tree <- xmlFlowToLTSlash
            pure $ UMarkup $ singletonOddA (ucontentComment cmt) <> tree)
    , ("&", do
        content <- xmlReference
        (<>) (UMarkup $ singletonOddA $ UContent $ singletonOddA content) <$> xmlFlowToLTSlash
        )
    , (">", lift . throwE . XMLStrayGT =<< gets snd)
    , ("\r", do
        postCRPos <- gets snd
        chooseFrom ((<>) (umarkupText (PositionRange startPos postCRPos) "\n") <$> xmlFlowToLTSlash)
          [ ("\n", do
              postNLPos <- gets snd
              (<>) (umarkupText (PositionRange startPos postNLPos) "\n") <$> xmlFlowToLTSlash)
          ])
    ]
  where
    normalChunk :: Parser UMarkup
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
      pure $ umarkupText (PositionRange startPos endPos) candidates <> rest

xmlReference :: Parser UText
xmlReference = do
  res <- chooseFrom refEnt [("#", refChar)]
  mandatory ";" $ XMLBadSyntax "Expecting ';' to terminate an entity or a character reference"
  pure res
  where
    refEnt = do
      start <- gets snd
      name <- xmlNCName
      end <- gets snd
      pure $ UText $ unitOddA (PositionRange start end, name)
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
          pure $ UText $ singletonOddA $ Seq.singleton (PositionRange start end, ST.singleton c)
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
