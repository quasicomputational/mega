module Q4C12.XML.Desc
  ( module Export
  , interleave
  )
  where

import Q4C12.TwoFinger (unconsOddA, consOddA, singletonOddA)

import Q4C12.XML.Desc.Class as Export
  ( Desc, Pos, El, OddFlow, EvenFlow, DT
  , evenUp, attrF, oddWS, oddTx, oddTxPos, nonTerminalEven, nonTerminalOdd
  , datatypeDT, tokenDT, nonTerminalE
  , uattrF
  , elementPosMixed, elementMixed, elementDT
  , flowEvenPreWS, flowWSE
  , naturalZeroDT, naturalOneDT, langDT, ncNameDT, stringTokenDT
  )
import Q4C12.XML.Desc.RApplicative as Export
  ( RFunctor (rfmap)
  , RPlus (rplus, rempty)
  , RPlusApplyR (ActionR, rconsR), rright
  , RAlternative (rnil, rmany, rsome), rcons
  , roptional
  )
import Q4C12.XML.Desc.TH as Export
  ( pat, patGroup, imposs, makeRPlus )

interleave
  :: (Desc tag)
  => El tag e
  -> OddFlow tag a
  -> OddFlow tag (TwoFingerOddA e a)
interleave el tx = rfmap (from doubleProd . iso f g) $ rconsR
  (rmany $ rfmap (from doubleProd) $ evenUp tx $ rfmap singleProd el)
  (rfmap singleProd tx)
  where
    f :: ([(a, e)], a) -> TwoFingerOddA e a
    f (pairs, a) = foldr (uncurry consOddA) (singletonOddA a) pairs
    g :: TwoFingerOddA e a -> ([(a, e)], a)
    g = unfoldr' uncons'
    uncons' :: TwoFingerOddA e a -> Either a ((a, e), TwoFingerOddA e a)
    uncons' tree = unconsOddA tree <&> \(a, e, tree') -> ((a, e), tree')
