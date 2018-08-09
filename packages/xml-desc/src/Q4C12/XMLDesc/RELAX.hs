{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Q4C12.XMLDesc.RELAX
  ( relax
  , RELAX
  ) where

import qualified Control.Monad.Trans.Accum as Accum
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.DList as DList
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Q4C12.XML
  ( QName (QName), Element, addUAttr, addUAttrS, rngelem
  , markupElement, markupText
  )

import Q4C12.XMLDesc.Class
  ( Desc ( DT, El, OddFlow, EvenFlow, Pos, Cmt, nonTerminalE, elementE, tokenDT
         , nonTerminalOdd, oddTxPos, oddTxNoPos, oddWS, nonTerminalEven, attrF, evenUp
         , elementEPos, datatypeDT
         )
  , Datatype (Datatype)
  , CompleteFlow (CompleteFlowDT, CompleteFlowMixed)
  )
import Q4C12.XMLDesc.RApplicative
  ( rfmap, RPlusApplyR (ActionR, rconsR), RPlus (rempty, rplus)
  , RAlternative (rnil, rsome, rmany)
  )

--TODO: is this a free applicative in disguise?
--TODO: statically enforce that RPFSequence cannot have direct RPFSequence children, R*Choice cannot have direct R*Choice children.
--TODO (maybe): Also iron out which way they distribute canonically, while I'm at it? (Distributing may lead to an explosion in the schema size, though.)
--TODO: this is too permissive: we can wind up with elements abutting on elements; is that actually a problem, though? Well, we might want to do a schema optimisation pass and ensuring correctness statically would be nice.
data RulePF
  = RPFNonTerminal LText
  | RPFSequence (Seq RulePF)
  | RPFChoice (Seq RulePF)
  | RPFMany RulePF
  | RPFSome RulePF
  | RPFEvenUp RuleCF RuleE
  | RPFAttr QName RuleDT
  deriving (Show)

data RuleTx = RTWhitespace | RTAny
  deriving (Show)

data RuleCF
  = RCFNonTerminal LText
  | RCFChoice (Seq RuleCF)
  | RCFFlow RulePF RuleCF
  | RCFText RuleTx
  deriving (Show)

data RuleE
  = REElemMixed QName RuleCF
  | REElemDT QName RuleDT
  | REChoice (Seq RuleE)
  | RENonTerminal LText
  deriving (Show)

data RuleDT
  = RDTDatatype LText LText
  | RDTToken LText
  | RDTChoice (Seq RuleDT)
  | RDTSequence (Seq RuleDT)
  | RDTMany RuleDT
  | RDTSome RuleDT
  deriving (Show)

--TODO: unify with SeenName.
data Productions = Productions
  !(DList (LText, RulePF))
  !(DList (LText, RuleCF))
  !(DList (LText, RuleE))

instance Semigroup Productions where
  Productions p1 c1 e1 <> Productions p2 c2 e2 =
    Productions (p1 <> p2) (c1 <> c2) (e1 <> e2)

instance Monoid Productions where
  mempty = Productions mempty mempty mempty
  mappend = (<>)

data RELAX

data DefinitionNamespace
  = DfnNsP
  | DfnNsC
  | DfnNsE
  deriving (Show, Eq, Ord)

dfnName :: DefinitionNamespace -> LText -> LText
dfnName DfnNsP = mappend "fp_"
dfnName DfnNsC = mappend "fc_"
dfnName DfnNsE = mappend "e_"

type SeenNames = Set (DefinitionNamespace, LText)

instance Desc RELAX where
  data EvenFlow RELAX _ = GrPF
    { runGrPF :: AccumT SeenNames (Writer Productions) RulePF }
    deriving (Functor)
  data OddFlow RELAX _ = GrCF
    { runGrCF :: AccumT SeenNames (Writer Productions) RuleCF }
    deriving (Functor)
  data El RELAX _ = GrE
    { runGrE :: AccumT SeenNames (Writer Productions) RuleE }
    deriving (Functor)
  data DT RELAX _ = GrT { runGrT :: RuleDT }
    deriving (Functor)
  type Pos RELAX = ()
  type Cmt RELAX = ()

  evenUp t e = GrPF $ RPFEvenUp <$> runGrCF t <*> runGrE e
  attrF qn (GrT t) = GrPF $ pure $ RPFAttr qn t

  oddTxPos = GrCF $ pure $ RCFText RTAny
  oddTxNoPos = oddTxPos
  oddWS = GrCF $ pure $ RCFText RTWhitespace

  --TODO: we can distinguish between might-be-recursive definitions, and definitely-not-recursive; the latter always indicates an error. (Er. Present-me wonders what past-me meant by that? It makes sense to name non-recursive things for schema neatness reasons.)
  --TODO: these currently punt the responsibility of making sure that each name is never incompatibly redefined to the user, but we could actually do that check ourselves.
  nonTerminalEven name a = GrPF $ do
    let qn = (DfnNsP, name)
    present <- Accum.looks (Set.member qn)
    unless present $ do
      Accum.add $ Set.singleton qn
      r <- runGrPF a
      lift $ Writer.tell $
        Productions (DList.singleton (name, r)) mempty mempty
    pure $ RPFNonTerminal name

  nonTerminalOdd name a = GrCF $ do
    let qn = (DfnNsC, name)
    present <- Accum.looks (Set.member qn)
    unless present $ do
      Accum.add $ Set.singleton qn
      r <- runGrCF a
      lift $ Writer.tell $
        Productions mempty (DList.singleton (name, r)) mempty
    pure $ RCFNonTerminal name

  datatypeDT (Datatype _ _ lib name) = GrT $ RDTDatatype lib name

  tokenDT = GrT . RDTToken

  elementEPos name body = rfmap (from dropUnit) $ elementE name body

  elementE name (CompleteFlowMixed a) =
    GrE $ REElemMixed name <$> runGrCF a
  elementE name (CompleteFlowDT a) =
    GrE $ pure $ REElemDT name (runGrT a)

  nonTerminalE name a = GrE $ do
    let qn = (DfnNsE, name)
    present <- Accum.looks (Set.member qn)
    unless present $ do
      Accum.add $ Set.singleton qn
      r <- runGrE a
      lift $ Writer.tell $
        Productions mempty mempty (DList.singleton (name, r))
    pure $ RENonTerminal name

instance Invariant (EvenFlow RELAX) where
  invmap = invmapFunctor

instance RPlus (EvenFlow RELAX) where
  rempty = GrPF $ pure $ RPFChoice mempty
  rplus a as = GrPF $ do
    results <- (,) <$> runGrPF a <*> runGrPF as
    pure $ RPFChoice $ case results of
      (RPFChoice xs, RPFChoice ys) -> xs <> ys
      (RPFChoice xs, x) -> x Seq.<| xs
      (x, RPFChoice xs) -> x Seq.<| xs
      (x, y) -> Seq.fromList [x, y]

instance RPlusApplyR (EvenFlow RELAX) where
  type ActionR (EvenFlow RELAX) = EvenFlow RELAX
  rconsR (GrPF a) (GrPF b) = GrPF $ do
    results <- (,) <$> a <*> b
    pure $ RPFSequence $ case results of
      (RPFSequence xs, RPFSequence ys) -> xs <> ys
      (RPFSequence xs, x) -> xs Seq.|> x
      (x, RPFSequence xs) -> x Seq.<| xs
      (x, y) -> Seq.fromList [x, y]

instance RAlternative (EvenFlow RELAX) where
  rnil = GrPF $ pure $ RPFSequence mempty
  rmany (GrPF a) = GrPF $ RPFMany <$> a
  rsome (GrPF a) = GrPF $ RPFSome <$> a

instance Invariant (OddFlow RELAX) where
  invmap = invmapFunctor

instance RPlus (OddFlow RELAX) where
  rempty = GrCF $ pure $ RCFChoice mempty
  rplus a as = GrCF $ do
    results <- (,) <$> runGrCF a <*> runGrCF as
    pure $ RCFChoice $ case results of
      (RCFChoice xs, RCFChoice ys) -> xs <> ys
      (RCFChoice xs, x) -> x Seq.<| xs
      (x, RCFChoice xs) -> x Seq.<| xs
      (x, y) -> Seq.fromList [x, y]

instance RPlusApplyR (OddFlow RELAX) where
  type ActionR (OddFlow RELAX) = EvenFlow RELAX
  rconsR f t = GrCF $ RCFFlow <$> runGrPF f <*> runGrCF t

instance Invariant (El RELAX) where
  invmap = invmapFunctor

instance RPlus (El RELAX) where
  rempty = GrE $ pure $ REChoice mempty
  rplus a as = GrE $ do
    results <- (,) <$> runGrE a <*> runGrE as
    pure $ REChoice $ case results of
      (REChoice xs, REChoice ys) -> xs <> ys
      (REChoice xs, y) -> y Seq.<| xs
      (x, REChoice ys) -> x Seq.<| ys
      (x, y) -> Seq.fromList [x, y]

instance Invariant (DT RELAX) where
  invmap = invmapFunctor

instance RPlus (DT RELAX) where
  rempty = GrT $ RDTChoice mempty
  rplus a as = GrT $ RDTChoice $
    case (runGrT a, runGrT as) of
      (RDTChoice xs, RDTChoice ys) -> xs <> ys
      (RDTChoice xs, y) -> y Seq.<| xs
      (x, RDTChoice ys) -> x Seq.<| ys
      (x, y) -> Seq.fromList [x, y]

instance RPlusApplyR (DT RELAX) where
  type ActionR (DT RELAX) = DT RELAX
  rconsR (GrT a) (GrT b) = GrT $ RDTSequence $ case (a, b) of
    (RDTSequence xs, RDTSequence ys) -> xs <> ys
    (RDTSequence xs, x) -> xs Seq.|> x
    (x, RDTSequence xs) -> x Seq.<| xs
    (x, y) -> Seq.fromList [x, y]

instance RAlternative (DT RELAX) where
  rnil = GrT $ RDTSequence mempty
  rmany (GrT a) = GrT $ RDTMany a
  rsome (GrT a) = GrT $ RDTSome a

relax :: El RELAX a -> Element cmt ()
relax p =
  let (start, prods) = runWriter $ evalAccumT (runGrE p) mempty
  in rngelem "grammar" $ foldMap markupElement $
       rngelem "start" (markupElement $ relaxRE start)
         : relaxProductions prods

--TODO: slightly more principled approach than this in re namespacing? The action-at-a-distance linkage here is worrisome.
relaxProductions :: Productions -> [Element cmt ()]
relaxProductions (Productions prodsP prodsC prodsE) =
  uncurry go <$> fold
    [ bimap ((,) DfnNsP) (f . relaxRPF) <$> toList prodsP
    , bimap ((,) DfnNsC) (f . relaxRCF) <$> toList prodsC
    , bimap ((,) DfnNsE) relaxRE <$> toList prodsE
    ]
  where
    f :: Maybe (Element cmt ()) -> Element cmt ()
    f = fromMaybe $ rngelem "empty" mempty
    go :: (DefinitionNamespace, LText) -> Element cmt () -> Element cmt ()
    go qn =
      addUAttr "name" (uncurry dfnName qn) . rngelem "define" . markupElement

splitMaybes :: [Maybe a] -> ([a], Bool)
splitMaybes as = (catMaybes as, any isNothing as)

addRelaxNameAttrs :: QName -> Element cmt () -> Element cmt ()
addRelaxNameAttrs (QName Nothing local) =
  addUAttrS "name" local
addRelaxNameAttrs (QName (Just ns) local) =
  addUAttrS "name" local . addUAttrS "ns" ns

--TODO: prune sequences involving notAllowed?
relaxRPF :: RulePF -> Maybe (Element cmt ())
relaxRPF (RPFNonTerminal name) = Just $
  addUAttr "name" (dfnName DfnNsP name) $ rngelem "ref" mempty
relaxRPF (RPFSequence rs) = case mapMaybe relaxRPF $ toList rs of
  [] -> Nothing
  [a] -> Just a
  as -> Just $ rngelem "group" $ foldMap markupElement as
relaxRPF (RPFMany a) = rngelem "zeroOrMore" . markupElement <$> relaxRPF a
relaxRPF (RPFSome a) = rngelem "oneOrMore" . markupElement <$> relaxRPF a
relaxRPF (RPFChoice rs) = relaxChoice $ relaxRPF <$> toList rs
relaxRPF (RPFEvenUp t e) = Just $ case relaxRCF t of
  Nothing -> relaxRE e
  Just a -> rngelem "group" $
    foldMap markupElement [a, relaxRE e]
relaxRPF (RPFAttr name tp) = Just $ addRelaxNameAttrs name $
  rngelem "attribute" $ markupElement $
    maybe (rngelem "empty" mempty) (rngelem "list" . markupElement) $
      relaxRDT tp

relaxRCF :: RuleCF -> Maybe (Element cmt ())
relaxRCF (RCFNonTerminal name) = Just $
  addUAttr "name" (dfnName DfnNsC name) $ rngelem "ref" mempty
relaxRCF (RCFChoice rs) = relaxChoice $ relaxRCF <$> toList rs
relaxRCF (RCFFlow partial complete) =
  case (relaxRPF partial, relaxRCF complete) of
    (Nothing, Nothing) -> Nothing
    (Just a, Nothing) -> Just a
    (Nothing, Just b) -> Just b
    (Just a, Just b) -> Just $ rngelem "group" $
      foldMap markupElement [a, b]
relaxRCF (RCFText t) = relaxRTx t

relaxRE :: RuleE -> Element cmt ()
relaxRE (RENonTerminal name) =
  addUAttr "name" (dfnName DfnNsE name) $ rngelem "ref" mempty
relaxRE (REChoice ras) = case relaxRE <$> toList ras of
  [] -> rngelem "notAllowed" mempty
  [a] -> a
  as -> rngelem "choice" $ foldMap markupElement as
relaxRE (REElemMixed name body) =
  addRelaxNameAttrs name $ rngelem "element" $ markupElement $
    fromMaybe (rngelem "empty" mempty) $ relaxRCF body
relaxRE (REElemDT name body) =
  addRelaxNameAttrs name $ rngelem "element" $ markupElement $
    maybe (rngelem "empty" mempty) (rngelem "list" . markupElement) $
      relaxRDT body

relaxRTx :: RuleTx -> Maybe (Element cmt ())
relaxRTx RTAny = Just $ rngelem "text" mempty
relaxRTx RTWhitespace = Nothing

relaxRDT :: RuleDT -> Maybe (Element cmt ())
relaxRDT (RDTDatatype lib name) = Just
  $ addUAttr "datatypeLibrary" lib $ addUAttr "type" name $ rngelem "data" mempty
relaxRDT (RDTSequence ts) = case mapMaybe relaxRDT $ toList ts of
  [] -> Nothing
  [a] -> Just a
  as -> Just $ rngelem "group" $ foldMap markupElement as
relaxRDT (RDTChoice ts) = relaxChoice $ toList $ relaxRDT <$> ts
relaxRDT (RDTMany t) = rngelem "zeroOrMore" . markupElement <$> relaxRDT t
relaxRDT (RDTSome t) = rngelem "oneOrMore" . markupElement <$> relaxRDT t
relaxRDT (RDTToken tok) = Just
  $ addUAttr "datatypeLibrary" ""
  $ addUAttr "type" "token" $ rngelem "value" $ markupText tok

relaxChoice :: [Maybe (Element cmt ())] -> Maybe (Element cmt ())
relaxChoice = go . splitMaybes
  where
    go :: ([Element cmt ()], Bool) -> Maybe (Element cmt ())
    go = \case
      (as, False) -> Just $ relaxChoiceNonEmpty as
      ([], True) -> Nothing
      ([a], True) -> Just $ rngelem "optional" $ markupElement a
      (as, True) -> Just $ rngelem "optional" $ markupElement $
        rngelem "choice" $ foldMap markupElement as

relaxChoiceNonEmpty :: [Element cmt ()] -> Element cmt ()
relaxChoiceNonEmpty [] = rngelem "notAllowed" mempty
relaxChoiceNonEmpty [a] = a
relaxChoiceNonEmpty as = rngelem "choice" $ foldMap markupElement as
