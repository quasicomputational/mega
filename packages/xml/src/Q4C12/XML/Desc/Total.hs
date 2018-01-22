{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Q4C12.XML.Desc.Total
  ( switch, _case, _group
  , makeIsos
  )
  where

import Q4C12.XML.Desc.Class (Desc, El, nonTerminalE)
import Q4C12.XML.Desc.RApplicative (RPlus, rfmap, rempty, rplus)

--TODO: issue with doing it this way: it's less efficient to inspect a HSum than a native sum type. Can we claw some of that back with unpacking?

data ConstructorInfo = ConstructorInfo Name [Type]

fromPatToExp :: Pat -> Q Exp
fromPatToExp (TH.LitP lit) = TH.litE lit
fromPatToExp (TH.VarP name) = TH.varE name
fromPatToExp (TH.TupP pats) = TH.tupE $ fromPatToExp <$> pats
fromPatToExp TH.WildP = fail "fromPatToExp: Cannot convert a wildcard pattern into an expression."
fromPatToExp (TH.ConP name pats) =
  foldl' TH.appE (TH.conE name) (fromPatToExp <$> pats)
fromPatToExp _ = fail "fromPatToExp: Unknown pattern type, oops!" --TODO: review other constructors for usefulness

--TODO: fill this out.
makeIsos :: Name -> Q [Dec]
makeIsos typeName = do
  info <- TH.reify typeName
  case info of
    TH.TyConI (TH.DataD ctx _ varBinders _ cons _) -> do
      let varNames = extractVarFromBinder <$> varBinders
          resType = foldl' TH.AppT (TH.ConT typeName) (TH.VarT <$> varNames)
          extractConstructorInfo (TH.NormalC conName bangTypes) =
            pure $ ConstructorInfo (snd <$> bangTypes)
          extractConstructorInfo _ = fail "makeIsos: only works on normal ADTs."
      cons <- traverse extractConstructorInfo cons
      let getGenericConstructor (ConstructorInfo _ fields) = do
            fieldNames <- traverse (const $ newName "x") fields
            let typ = AppT (PromotedT 'Just) $ AppT (ConT ''HProd) $ foldr (\h t -> AppT (AppT PromotedConsT h) t) PromotedNilT fields
                fr = ConP conName (VarP <$> fieldNames)
                to = ConP 'VoidableJust [foldr (\h t -> ConE 'HProdCons [h, t]) (conE 'HProdNil) fieldNames]
            pure $ HProdCons typ $ HProdCons fr $ HProdCons to HProdNil
      genericData <- traverse getGenericConstructor cons
      let genericType = AppT (AppT (ConT ''HSumF) ''Voidable) $ foldr (\h t -> AppT (AppT PromotedConsT (AppT (PromotedT 'Just) h)) t) PromotedNilT (view headL <$> genericData)
      genericFrom <- lamCaseE $ genericData <&> \(HProdCons _ (HProdCons frPat (HProdCons toPat HProdNil))) -> do
        exp <- fromPatToExp toPat
        match frPat (normalB exp) []
      genericTo <- lamCaseE $ genericData <&> \(HProdCons _ (HProdCons frPat (HProdCons toPat HProdNil))) -> do
        exp <- fromPatToExp frPat
        match toPat (normalB exp) []
      let genericName = mkName $ "_" <> typeName
      pure [ SigD genericName $ AppT (AppT (ConT ''Iso') resType) genericType
           , FunD genericName $ Clause [] (AppE (AppE (VarE 'iso) genericFrom) genericTo)
           ]
    _ -> fail "makeIsos: unknown result from reify."

--TODO: define what happens here wrt ordering and overlap?
_case :: (RPlus f) => Iso' s (Either t a) -> f a -> f t -> f s
_case p fa ft = rfmap (from eitherSum . from p) $ rplus ft $ rplus fa $ rempty

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
