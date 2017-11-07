{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Q4C12.XML.Desc.Class
  ( Desc (..)
  , Datatype (..)
  , uattrF
  , elementPosMixed, elementMixed, elementDT
  , flowEvenPreWS, flowWSE
  , naturalZeroDT, naturalOneDT, langDT, ncNameDT, stringTokenDT
  , CompleteFlow (..)
  )
  where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import qualified Data.Text.Lazy.Read as LTR

import Q4C12.XML (QName, uname)
import Q4C12.XML.Desc.RApplicative
  ( RFunctor (rfmap), RPlus, RPlusApplyR, ActionR, rconsR
  , RAlternative
  )

--TODO: we can actually put these in <choice>. And I guess name them with a nonTerminalCF or something.
--TODO: support adding attributes to any CompleteFlow
data CompleteFlow tag a
  = CompleteFlowMixed (OddFlow tag a)
  | CompleteFlowDT (DT tag a)

instance (Desc tag) => RFunctor (CompleteFlow tag) where
  rfmap f (CompleteFlowMixed prs) = CompleteFlowMixed (rfmap f prs)
  rfmap f (CompleteFlowDT prs) = CompleteFlowDT (rfmap f prs)

elementPosMixed
  :: (Desc tag)
  => QName
  -> OddFlow tag (HProd as)
  -> El tag (HProd (Pos tag ': as))
elementPosMixed qn = elementEPos qn . CompleteFlowMixed

elementMixed :: (Desc tag) => QName -> OddFlow tag a -> El tag a
elementMixed qn = elementE qn . CompleteFlowMixed

--TODO: this should be able to take attrs.
elementDT :: (Desc tag) => QName -> DT tag a -> El tag a
elementDT qn = elementE qn . CompleteFlowDT

--Invariants: For (Datatype f g _ _) and some endomorphism h: f . g . h === fmap h; if f a = Right x, then g x === a; if a == b, then g a == g b. (TODO: that last law doesn't seem right.) (TODO: I think we want to put a 'Right' back in that first law, maybe?) (TODO: compare with Prisms... Ooh, or 'affine traversals'?)
data Datatype a = Datatype (LText -> Either LText a) (a -> LText) LText LText

naturalZeroDT :: (Desc tag) => DT tag Natural
naturalZeroDT = datatypeDT $ Datatype prs prnt "http://www.w3.org/2001/XMLSchema-datatypes" "nonNegativeInteger"
  where
    prs :: LText -> Either LText Natural
    prs t = do
      --TODO: check for isSpace correctness, also that stripStart...
      (n, rest) <- first LT.pack $ LTR.decimal $ LT.stripStart t
      unless (LT.all isSpace rest) $ Left "trailing non-decimals"
      Right n
    prnt :: Natural -> LText
    prnt = LTB.toLazyText . LTBI.decimal

--Returns the offset from 1 (i.e., reading '1' => 0.)
naturalOneDT :: (Desc tag) => DT tag Natural
naturalOneDT = datatypeDT $ Datatype prs prnt "http://www.w3.org/2001/XMLSchema-datatypes" "positiveInteger"
  where
    prs :: LText -> Either LText Natural
    prs t = do
      --TODO: check for isSpace correctness, also that stripStart...
      (n, rest) <- first LT.pack $ LTR.decimal $ LT.stripStart t
      unless (LT.all isSpace rest) $ Left "trailing non-decimals"
      when (n == 0) $ Left "zero is not an xsd:positiveInteger"
      Right (n - 1)
    prnt :: Natural -> LText
    prnt = LTB.toLazyText . LTBI.decimal . (+ 1)

--TODO: wrap up in an opaque type
langDT :: (Desc tag) => DT tag LText
langDT = datatypeDT $ Datatype prs prnt "http://www.w3.org/2001/XMLSchema-datatypes" "language"
  where
  --TODO: RFC 5646-compliant parsing; possibly also c14n?
    prs :: LText -> Either e LText
    prs = Right
    prnt :: LText -> LText
    prnt = id

--TODO: also opaquify?
ncNameDT :: (Desc tag) => DT tag LText
ncNameDT = datatypeDT $ Datatype prs prnt "http://www.w3.org/2001/XMLSchema-datatypes" "NCName"
  where
    --TODO: actually check this matches
    prs :: LText -> Either e LText
    prs = Right
    prnt :: LText -> LText
    prnt = id

stringTokenDT :: (Desc tag) => DT tag LText
stringTokenDT = datatypeDT $ Datatype Right id "" "token"

--Note: in RELAX NG, recursion has to happen through an element.
--Further note: our implementation of datatypes does not care for anything that wants 'raw' whitespace---it *will* be normalised. Hence we only offer things that do not contain whitespace.
--TODO: we currently offer matching patterns like (text, element)*, text; we could alternatively offer text, (element, text)*.
--TODO: rename EvenFlow to EvenPreFlow (matching (text, element)*) and add EvenPostFlow (matching (element, text)*).
--TODO: we could even have OddFlowE, matching (element, (text, element)*). (Note: we can't repurpose El for this, because we need El to provide the top-level entry point...)
--TODO: would like to be able to have nonTerminalDT.
--TODO: name class support, which presumably means another associated type with a Plus instance (and probably another nonTerminal?)
--TODO: consider making the DT bits return strict texts?
--TODO: implement a fourth instance, a linter. Preliminary lints to find: overlapping patterns (ambiguity). Hmm, but maybe that should be part of the RELAX instance, because that's got its own AST?
--TODO: position information in datatype thingies.
class (RAlternative (EvenFlow tag), RPlus (El tag), RAlternative (DT tag),
       RPlusApplyR (OddFlow tag), ActionR (OddFlow tag) ~ EvenFlow tag)
   => Desc tag where
  data EvenFlow tag :: * -> *
  data OddFlow tag :: * -> *
  data El tag :: * -> *
  data DT tag :: * -> *
  type Pos tag :: *

  evenUp
    :: OddFlow tag a
    -> El tag (HProd as)
    -> EvenFlow tag (HProd (a ': as))
  attrF :: QName -> DT tag a -> EvenFlow tag a

  oddWS :: OddFlow tag ()
  oddTx :: OddFlow tag LText
  oddTxPos :: OddFlow tag (Seq (Pos tag, LText))

  --TODO: should the labels be SText?
  nonTerminalEven :: LText -> EvenFlow tag a -> EvenFlow tag a
  nonTerminalOdd :: LText -> OddFlow tag a -> OddFlow tag a

  datatypeDT :: Datatype a -> DT tag a

  --Note: must not contain whitespace! Also, take care with the empty string.
  --TODO: replace this with something like valueDT :: (Eq a) => Datatype a -> a -> DT tag ()?
  tokenDT :: LText -> DT tag ()

  elementE :: QName -> CompleteFlow tag a -> El tag a
  default elementE :: (Pos tag ~ ()) => QName -> CompleteFlow tag a -> El tag a
  elementE qn
    = rfmap (dropUnit . from singleProd)
    . elementEPos qn
    . rfmap singleProd

  elementEPos
    :: QName
    -> CompleteFlow tag (HProd as)
    -> El tag (HProd (Pos tag ': as))
  nonTerminalE :: LText -> El tag a -> El tag a

flowWSE :: (Desc tag) => El tag a -> EvenFlow tag a
flowWSE
  = rfmap (dropUnit . from singleProd)
  . evenUp oddWS
  . rfmap singleProd

uattrF :: (Desc tag) => SText -> DT tag a -> EvenFlow tag a
uattrF = attrF . uname

flowEvenPreWS :: (Desc tag) => EvenFlow tag a -> OddFlow tag a
flowEvenPreWS p =
  rfmap (from singleProd) $ rconsR p (rfmap unitProd oddWS)
