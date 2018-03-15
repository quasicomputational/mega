{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
--TODO: really should be able to get away with just TemplateHaskellQuotes
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Q4C12.XML.Desc.Total
  ( switch, _case, _group
  , makeIsos
  )
  where

import Language.Haskell.TH (Q, Exp, Dec (DataD), Pat (LitP, VarP, TupP, WildP, ConP), Type, Con (NormalC), Name, Info (TyConI), TyVarBndr (PlainTV, KindedTV), reify, mkName, newName, nameBase)
import qualified Language.Haskell.TH.Lib as TH

import Q4C12.XML.Desc.Class (Desc, El, nonTerminalE)
import Q4C12.XML.Desc.RApplicative (RPlus, rfmap, rempty, rplus)

--TODO: issue with doing it this way: it's less efficient to inspect a HSum than a native sum type. Can we claw some of that back with unpacking?

fromPatToExp :: Pat -> Q Exp
fromPatToExp (LitP lit) = TH.litE lit
fromPatToExp (VarP name) = TH.varE name
fromPatToExp (TupP pats) = TH.tupE $ fromPatToExp <$> pats
fromPatToExp WildP = fail "fromPatToExp: Cannot convert a wildcard pattern into an expression."
fromPatToExp (ConP name pats) =
  foldl' TH.appE (TH.conE name) (fromPatToExp <$> pats)
fromPatToExp _ = fail "fromPatToExp: Unknown pattern type, oops!" --TODO: review other constructors for usefulness

getGenericConstructor :: Con -> Q (Name, Q Type, Q Pat, Q Pat)
getGenericConstructor (NormalC conName bangTypes) = do
  fieldNames <- traverse (const $ newName "x") bangTypes
  let fields = pure . snd <$> bangTypes
      typ = [t| HProd $(foldr (\h t -> [t| $h ': $t |]) TH.promotedNilT fields) |]
      fr = TH.conP conName (TH.varP <$> fieldNames)
      to = [p| VoidableJust $(foldr (\h t -> [p| HProdCons $(TH.varP h) $t |]) [p| HProdNil |] fieldNames) |]
  pure (conName, typ, fr, to)
getGenericConstructor _ =
  fail "getGenericConstructor: only works on normal ADTs."

extractVarFromBinder :: TyVarBndr -> Name
extractVarFromBinder (PlainTV name) = name
extractVarFromBinder (KindedTV name _) = name

snd4L :: Lens (a, x, b, c) (a, x', b, c) x x'
snd4L f (a, x, b, c) = f x <&> \x' -> (a, x', b, c)

--Generates things of the shape _Foo :: Iso' Foo (HSumF Voidable ['Just FooA, 'Just 'FooB])
makeToGeneric :: Name -> [Name] -> [(a, Q Type, Q Pat, Q Pat)] -> Q [Dec]
makeToGeneric typeName varNames genericData = do
  let genericType = [t| HSumF Voidable $(foldr (\h t -> [t| 'Just $h ': $t |]) TH.promotedNilT (view snd4L <$> genericData)) |]
      genericFrom = TH.lamCaseE $ genericData <&> \(_, _, frPat, toPat) ->
        TH.match frPat (TH.normalB $ fromPatToExp =<< toPat) []
      genericTo = TH.lamCaseE $ genericData <&> \(_, _, frPat, toPat) ->
        TH.match toPat (TH.normalB $ fromPatToExp =<< frPat) []
      genericName = mkName $ "_" <> nameBase typeName
      resType = foldl' TH.appT (TH.conT typeName) (TH.varT <$> varNames)
  sequence
    [ TH.sigD genericName [t| Iso' $resType $genericType |]
    , TH.funD genericName [TH.clause [] (TH.normalB [e| iso $genericFrom $genericTo |]) []]
    ]

makeSelectors :: [(Name, Q Type, a, b)] -> Q [Dec]
makeSelectors = _

makeIsos :: Name -> Q [Dec]
makeIsos typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ varBinders _ cons _) -> do
      genericData <- traverse getGenericConstructor cons
      let varNames = extractVarFromBinder <$> varBinders
      foldSequence
        [ makeToGeneric typeName varNames genericData
        , makeSelectors genericData
        ]
    _ -> fail "makeIsos: unknown result from reify."

--TODO: define what happens here wrt ordering and overlap?
_case :: (RPlus f) => Iso' s (Either t a) -> f a -> f t -> f s
_case p fa ft = rfmap (from eitherSum . from p) $ rplus ft $ rplus fa $ rempty

--TODO: generalise from just El to... well, I guess I need a 'Labellable' typeclass now?
_group :: (Desc tag, VoidableMaybeList as, MergeableLists as bs) => LText -> (El tag (HSumF Voidable (NothingList as)) -> El tag (HSumF Voidable as)) -> El tag (HSumF Voidable bs) -> El tag (HSumF Voidable (MergedLists as bs))
_group label f a = rfmap voidableHSums $ rplus (nonTerminalE label $ f $ rfmap (iso absurdHSum (absurdVoidedMaybeHSumF (pr f))) rempty) $ rplus a rempty
  where
    pr :: (x -> f (proxy as)) -> Proxy as
    pr _ = Proxy

switch :: (RPlus f, VoidableList as) => Iso' (HSum as) r -> (f (HSumF Voidable (NothingList as)) -> f (HSumF Voidable (JustList as))) -> f r
switch p f = rfmap (justVoidableSum . p) $ f $ rfmap (iso absurdHSum (absurdVoidedHSumF (pr p))) rempty
  where
    pr :: Iso' (HSum as) r -> Proxy as
    pr _ = Proxy

data Voidable :: Maybe * -> * where
  VoidableJust :: a -> Voidable ('Just a)

absurdVoidable :: Voidable 'Nothing -> a
absurdVoidable v = case v of {}

getVoidableJust :: Voidable ('Just a) -> a
getVoidableJust (VoidableJust a) = a

class VoidableMaybeList (as :: [Maybe *]) where
  absurdVoidedMaybeHSumF :: proxy as -> HSumF Voidable (NothingList as) -> x

instance VoidableMaybeList '[] where
  absurdVoidedMaybeHSumF _ = absurdHSumF

instance (VoidableMaybeList as) => VoidableMaybeList (a ': as) where
  absurdVoidedMaybeHSumF pr = eliminateHSumF absurdVoidable (absurdVoidedMaybeHSumF $ proxyTail pr)

type family NothingList (as :: [k]) :: [Maybe k'] where
  NothingList '[] = '[]
  NothingList (_ ': as) = 'Nothing ': NothingList as

justVoidableSum :: (VoidableList as) => Iso' (HSumF Voidable (JustList as)) (HSum as)
justVoidableSum = iso justVoidableHSumF justVoidableHSum

class VoidableList (as :: [*]) where
  type JustList as :: [Maybe *]
  absurdVoidedHSumF :: proxy as -> HSumF Voidable (NothingList as) -> x
  justVoidableHSumF :: HSumF Voidable (JustList as) -> HSum as
  justVoidableHSum :: HSum as -> HSumF Voidable (JustList as)

instance VoidableList '[] where
  type JustList '[] = '[]
  absurdVoidedHSumF _ = absurdHSumF
  justVoidableHSumF = absurdHSumF
  justVoidableHSum = absurdHSum

instance (VoidableList as) => VoidableList (a ': as) where
  type JustList (a ': as) = 'Just a ': JustList as
  absurdVoidedHSumF pr = eliminateHSumF absurdVoidable (absurdVoidedHSumF $ proxyTail pr)
  justVoidableHSumF = eliminateHSumF (HSumHere . getVoidableJust) (HSumThere . justVoidableHSumF)
  justVoidableHSum = eliminateHSum (HSumFHere . VoidableJust) (HSumFThere . justVoidableHSum)

proxyTail :: proxy (a ': as) -> Proxy as
proxyTail _ = Proxy

voidableHSums :: (MergeableLists as bs) => Iso' (HSum '[HSumF Voidable as, HSumF Voidable bs]) (HSumF Voidable (MergedLists as bs))
voidableHSums = iso mergeVoidableHSums splitVoidableHSums

mergeVoidableHSums :: (MergeableLists as bs) => HSum '[HSumF Voidable as, HSumF Voidable bs] -> HSumF Voidable (MergedLists as bs)
mergeVoidableHSums x@(HSumHere as) = liftMergedFirst as (pr x)
  where
    pr :: HSum '[x, HSumF f bs] -> Proxy bs
    pr _ = Proxy
mergeVoidableHSums x@(HSumThere (HSumHere bs)) = liftMergedSecond (pr x) bs
  where
    pr :: HSum '[HSumF f as, x] -> Proxy as
    pr _ = Proxy
mergeVoidableHSums (HSumThere (HSumThere xs)) = absurdHSum xs

class MergeableLists (as :: [Maybe *]) (bs :: [Maybe *]) where
  type MergedLists as bs :: [Maybe *]
  liftMergedFirst :: HSumF Voidable as -> proxy bs -> HSumF Voidable (MergedLists as bs)
  liftMergedSecond :: proxy as -> HSumF Voidable bs -> HSumF Voidable (MergedLists as bs)
  splitVoidableHSums :: HSumF Voidable (MergedLists as bs) -> HSum '[HSumF Voidable as, HSumF Voidable bs]

instance MergeableLists '[] '[] where
  type MergedLists '[] '[] = '[]
  liftMergedFirst x _ = absurdHSumF x
  liftMergedSecond _ x = absurdHSumF x
  splitVoidableHSums = absurdHSumF

instance (MergeableLists as bs) => MergeableLists ('Nothing ': as) ('Just b ': bs) where
  type MergedLists ('Nothing ': as) ('Just b ': bs) = 'Just b ': MergedLists as bs

  liftMergedFirst (HSumFHere a) _ = case a of {}
  liftMergedFirst (HSumFThere as) pr = HSumFThere $ liftMergedFirst as (proxyTail pr)

  liftMergedSecond _ (HSumFHere b) = HSumFHere b
  liftMergedSecond pr (HSumFThere bs) = HSumFThere $ liftMergedSecond (proxyTail pr) bs

  splitVoidableHSums (HSumFHere b) = HSumThere $ HSumHere $ HSumFHere b
  splitVoidableHSums (HSumFThere xs) = splitVoidableHSums xs &
    ( eliminateHSum (HSumHere . HSumFThere)
    $ eliminateHSum (HSumThere . HSumHere . HSumFThere)
    $ absurdHSum
    )

instance (MergeableLists as bs) => MergeableLists ('Just a ': as) ('Nothing ': bs) where
  type MergedLists ('Just a ': as) ('Nothing ': bs) = 'Just a ': MergedLists as bs

  liftMergedFirst (HSumFHere a) _ = HSumFHere a
  liftMergedFirst (HSumFThere as) pr = HSumFThere $ liftMergedFirst as (proxyTail pr)

  liftMergedSecond _ (HSumFHere b) = case b of {}
  liftMergedSecond pr (HSumFThere bs) = HSumFThere $ liftMergedSecond (proxyTail pr) bs

  splitVoidableHSums (HSumFHere a) = HSumHere $ HSumFHere a
  splitVoidableHSums (HSumFThere xs) = splitVoidableHSums xs &
    ( eliminateHSum (HSumHere . HSumFThere)
    $ eliminateHSum (HSumThere . HSumHere . HSumFThere)
    $ absurdHSum
    )

instance (MergeableLists as bs) => MergeableLists ('Nothing ': as) ('Nothing ': bs) where
  type MergedLists ('Nothing ': as) ('Nothing ': bs) = 'Nothing ': MergedLists as bs

  liftMergedFirst (HSumFHere a) _ = case a of {}
  liftMergedFirst (HSumFThere as) pr = HSumFThere $ liftMergedFirst as (proxyTail pr)

  liftMergedSecond _ (HSumFHere b) = case b of {}
  liftMergedSecond pr (HSumFThere bs) = HSumFThere $ liftMergedSecond (proxyTail pr) bs

  splitVoidableHSums (HSumFHere x) = case x of {}
  splitVoidableHSums (HSumFThere xs) = splitVoidableHSums xs &
    ( eliminateHSum (HSumHere . HSumFThere)
    $ eliminateHSum (HSumThere . HSumHere . HSumFThere)
    $ absurdHSum
    )
