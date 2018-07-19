{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Q4C12.XMLDesc.Print
  ( printEl
  , Print
  ) where

import qualified Data.DList as DList
import qualified Data.Map as Map
import Q4C12.TwoFinger (appendEvenAOddA, snocEvenA, singletonOddA, halfunsnocOddA)
import Q4C12.XML (QName, Element, addAttrs, element, Markup (Markup), markupText, Comment, Content, contentComment)

import Q4C12.XMLDesc.Class
  ( Desc ( DT, El, OddFlow, EvenFlow, Pos, Cmt, nonTerminalE, elementE, tokenDT
         , nonTerminalOdd, oddTxPos, oddTxNoPos, oddWS, nonTerminalEven, attrF, evenUp
         , elementEPos, datatypeDT
         )
  , Datatype (Datatype)
  , CompleteFlow (CompleteFlowDT, CompleteFlowMixed)
  )
import Q4C12.XMLDesc.RApplicative
  ( RFunctor (rfmap), RPlusApplyR (ActionR, rconsR), RPlus (rempty, rplus)
  , RAlternative (rnil)
  )

--TODO: pretty-printing? Would probably mean changing Desc (possibly only TxWS?).
--TODO: generate an xml-conduit Event stream directly? Alternatively, we could have another module that does that...

data Print

instance Desc Print where
  data EvenFlow Print a = PrintEF
    { runPrintEF :: a -> (TwoFingerEvenA (Element (Comment ()) ()) (Content (Comment ()) ()), Map QName LText) }
  data OddFlow Print a = PrintOF
    { runPrintOF :: a -> (TwoFingerOddA (Element (Comment ()) ()) (Content (Comment ()) ()), Map QName LText) }
  data El Print a = PrintEl { runPrintEl :: a -> Element (Comment ()) () }
  data DT Print a = PrintDT { runPrintDT :: a -> DList LText }
  type Pos Print = ()
  type Cmt Print = Comment ()

  evenUp (PrintOF f) (PrintEl g) = PrintEF $
    \(HProdCons a as) ->
      let (ilv, attrs) = f a
          e = g as
          (pairs, t) = halfunsnocOddA ilv
      in (snocEvenA pairs t e, attrs)

  attrF name (PrintDT f) = PrintEF $ \a ->
    (mempty, Map.singleton name $ intercalate0 " " $ f a)

  oddWS = PrintOF $ \ comments -> (singletonOddA $ foldMap contentComment comments, mempty)
  oddTxPos = PrintOF $ \ a -> (singletonOddA a, mempty)
  oddTxNoPos = oddTxPos

  nonTerminalEven _ = id
  nonTerminalOdd _ = id
  nonTerminalE _ = id

  datatypeDT (Datatype _ f _ _) = PrintDT $ DList.singleton . f

  tokenDT t = PrintDT $ \() -> DList.singleton t

  elementEPos name body = rfmap (from dropUnit) $ elementE name body

  elementE name (CompleteFlowMixed (PrintOF f)) = PrintEl $ \a ->
    let (ilv, attrs) = f a
    in addAttrs attrs $ element name $ Markup ilv
  elementE name (CompleteFlowDT (PrintDT f)) = PrintEl $
    element name . markupText . intercalate0 " " . f

instance RFunctor (EvenFlow Print) where
  rfmap i (PrintEF a) = PrintEF $ a . view (from i)

instance RPlus (EvenFlow Print) where
  rempty = PrintEF $ \case {}
  rplus a as = PrintEF $ \case
    HSumHere x -> runPrintEF a x
    HSumThere xs -> runPrintEF as xs

instance RPlusApplyR (EvenFlow Print) where
  type ActionR (EvenFlow Print) = EvenFlow Print
  rconsR (PrintEF f) (PrintEF g) = PrintEF $
    \(HProdCons a as) -> f a <> g as

instance RAlternative (EvenFlow Print) where
  rnil = PrintEF $ \HProdNil -> (mempty, mempty)

instance RFunctor (OddFlow Print) where
  rfmap i (PrintOF a) = PrintOF $ a . view (from i)

instance RPlus (OddFlow Print) where
  rempty = PrintOF $ \case {}
  rplus a as = PrintOF $ \case
    HSumHere x -> runPrintOF a x
    HSumThere xs -> runPrintOF as xs

instance RPlusApplyR (OddFlow Print) where
  type ActionR (OddFlow Print) = EvenFlow Print
  rconsR (PrintEF f) (PrintOF g) = PrintOF $
    \(HProdCons a as) ->
      let (pairs, attrs) = f a
          (ilv, attrs') = g as
      in (appendEvenAOddA pairs ilv, attrs <> attrs')

instance RFunctor (El Print) where
  rfmap i (PrintEl a) = PrintEl $ a . view (from i)

instance RPlus (El Print) where
  rempty = PrintEl $ \case {}
  rplus a as = PrintEl $ \case
    HSumHere x -> runPrintEl a x
    HSumThere xs -> runPrintEl as xs

instance RFunctor (DT Print) where
  rfmap i (PrintDT a) = PrintDT $ a . view (from i)

instance RPlus (DT Print) where
  rempty = PrintDT $ \case {}
  rplus a as = PrintDT $ \case
    HSumHere x -> runPrintDT a x
    HSumThere xs -> runPrintDT as xs

instance RPlusApplyR (DT Print) where
  type ActionR (DT Print) = DT Print
  rconsR (PrintDT f) (PrintDT g) = PrintDT $
    \(HProdCons a as) -> f a <> g as

instance RAlternative (DT Print) where
  rnil = PrintDT $ \HProdNil -> mempty

printEl :: El Print a -> a -> Element (Comment ()) ()
printEl = runPrintEl
