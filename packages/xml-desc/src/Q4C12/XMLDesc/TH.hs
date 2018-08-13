{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
--TODO: we don't need the full weight of TemplateHaskell, surely---we could get away with just TemplateHaskellQuotes? Except GHC doesn't like splices inside quotes, despite that still only being sugar...
module Q4C12.XMLDesc.TH
  ( pat, patGroup, imposs
  , makeRPlus
  , fromPatToExp
  , CasePat, CaseImposs
  , AcceptsVars, Ret, withWildcards
  )
  where

import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.DList as DList
import qualified Data.List as List
import Language.Haskell.TH (Q, Exp, Pat, Name)
import qualified Language.Haskell.TH as TH

import Q4C12.XMLDesc.RApplicative (rfmap, rplus, rempty)

--TODO: separate this code's two functions: one, the iso generation; and two, the rplus generation. This will come in handy for {unit, single, double}Prod, adding tuples to the left/right side of things, etc. (Note to self: that will likely require renaming imposs to impossR, and adding an impossL.)

pat
  :: (AcceptsVars f, Ret f ~ Q Pat)
  => Q Exp
  -> f
  -> WriterT (DList (Either c CasePat)) Q ()
pat e f = do
  (p, names) <- lift $ withVars f
  Writer.tell $ DList.singleton $ Right $ CasePat p names e

patGroup
  :: (Monad m)
  => Q Exp
  -> WriterT (DList (Either c CasePat)) m r
  -> WriterT (DList (Either c CasePat)) m r
patGroup name = Writer.censor $ \dl -> DList.fromList $
  let (is, ps) = partitionEithers $ toList dl
  in Right (CasePatGroup name ps) : fmap Left is

class AcceptsVars f where
  type Ret f :: Type
  type instance Ret f = f

  withVars :: f -> Q (Ret f, [Name])
  default withVars :: (Ret f ~ f) => f -> Q (Ret f, [Name])
  withVars r = pure (r, [])

  withWildcards :: f -> Ret f
  default withWildcards :: (Ret f ~ f) => f -> Ret f
  withWildcards = id

instance AcceptsVars (Q a)

instance AcceptsVars (a, b)

instance (a ~ Q Pat, AcceptsVars r) => AcceptsVars (a -> r) where
  type Ret (a -> r) = Ret r
  withVars f = do
    name <- TH.newName "x"
    (p, vs) <- withVars (f $ TH.varP name)
    pure (p, name:vs)
  withWildcards f = withWildcards (f TH.wildP)

data CasePat
  = CasePat (Q Pat) [Name] (Q Exp)
  | CasePatGroup (Q Exp) [CasePat]

data CaseImposs = CaseImposs (Q Pat) (Q Exp)

imposs
  :: (AcceptsVars f, Ret f ~ (Q Pat, Q Exp))
  => f
  -> WriterT (DList (Either CaseImposs c)) Q ()
imposs f = do
  ((p, e), _) <- lift $ withVars f
  Writer.tell $ DList.singleton $ Left $ CaseImposs p e

--TODO: exporting this from here feels kind of weird; move to Util.hs?
fromPatToExp :: Pat -> Q Exp
fromPatToExp (TH.LitP lit) = TH.litE lit
fromPatToExp (TH.VarP name) = TH.varE name
fromPatToExp (TH.TupP pats) = TH.tupE $ fromPatToExp <$> pats
fromPatToExp TH.WildP = fail "fromPatToExp: Cannot convert a wildcard pattern into an expression."
fromPatToExp (TH.ConP name pats) =
  foldl' TH.appE (TH.conE name) (fromPatToExp <$> pats)
fromPatToExp _ = fail "fromPatToExp: Unknown pattern type, oops!" --TODO: review other constructors for usefulness

--TODO: dlist? Seq?
data IsoConstruction a b = IsoConstruction
  [(a, b)] [(b, Q Exp)] [(a, Q Exp)]

instance Semigroup (IsoConstruction a b) where
  IsoConstruction a1 b1 c1 <> IsoConstruction a2 b2 c2 =
    IsoConstruction (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid (IsoConstruction a b) where
  mempty = IsoConstruction mempty mempty mempty
  mappend = (<>)

instance Bifunctor IsoConstruction where
  bimap f g (IsoConstruction ms r2l l2r) =
    IsoConstruction (bimap f g <$> ms) (first g <$> r2l) (first f <$> l2r)

makeIso' :: IsoConstruction (Q Pat) (Q Pat) -> Q Exp
makeIso' (IsoConstruction ms r2l l2r) = do
  let lhs = l2r <> fmap (fmap (>>= fromPatToExp)) ms
      rhs = r2l <> fmap (fmap (>>= fromPatToExp) . swap) ms
  [| iso $(TH.lamCaseE $ makeMatch <$> lhs)
       $(TH.lamCaseE $ makeMatch <$> rhs) |]
  where
    makeMatch (p, e) = TH.match p (TH.normalB e) []

-- Scheme:
--    [pat C1 V1 B1, pat C2 V2 B2, imposs C3 X3 ...] ==> rfmap (iso (Here V1 -> C1 ; There (Here V2) -> C2 ...) (C1 -> Here V1 ; C2 -> There (Here V2); C3 -> X3 ...)) $ rplus (B1 : B2 ...)
makeRPlus :: WriterT (DList (Either CaseImposs CasePat)) Q () -> Q Exp
makeRPlus a = execWriterT a >>= \dl -> do
  let (imposses, pats) = partitionEithers $ toList dl
  (isoConstruction, e) <- collectCasePats pats
  let isoConstruction' = isoConstruction <>
        foldMap caseImpossToIsoConstruction imposses
  [e| rfmap $(makeIso' isoConstruction') $e |]
  where
    caseImpossToIsoConstruction :: CaseImposs -> IsoConstruction a (Q Pat)
    caseImpossToIsoConstruction (CaseImposs p x) =
      IsoConstruction [] [(p, x)] []

    thereStep :: (a -> Q Pat) -> a -> Q Pat
    thereStep f p = [p| HSumThere $(f p) |]

    rplusStep :: Q Exp -> Q Exp -> Q Exp
    rplusStep e t = [e| rplus $e $t |]

    step :: CasePat -> Q (IsoConstruction (Q Pat) (Q Pat), Q Exp)
    step (CasePat p names e) = do
      let vs = foldr (\n ns -> [p| HProdCons $(TH.varP n) $ns |])
                 [p| HProdNil |] names
      pure (IsoConstruction [(vs, p)] [] [], e)
    step (CasePatGroup name ps) = do
      let label e = [e| nonTerminalE $name $e |]
      fmap label <$> collectCasePats ps

    collectCasePats :: [CasePat] -> Q (IsoConstruction (Q Pat) (Q Pat), Q Exp)
    collectCasePats ps = do
      (isoConstructions, exps) <- List.unzip <$> traverse step ps
      n <- TH.newName "x"
      let --Prove to GHC that we've been exhaustive: after we check every Here, there's no There left except on the bottom.
          final = IsoConstruction [] []
            [(TH.varP n, [e| absurdHSum $(TH.varE n) |])]
          isoConstructions' = List.zipWith first (iterate thereStep id) $
            (first (\p -> [p| HSumHere $p |]) <$> isoConstructions) <> [final]
      pure ( fold isoConstructions'
           , foldr rplusStep [e| rempty |] exps
           )
