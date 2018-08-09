module Q4C12.XMLDesc
  ( module Export
  , interleave
  , oddWSDropComments
  , flowEvenPreWSDropComments
  , flowWSEDropComments
  )
  where

import Q4C12.TwoFinger (unconsOddA, consOddA, singletonOddA)

import Q4C12.XMLDesc.Class as Export
  ( Desc, Pos, Cmt, El, OddFlow, EvenFlow, DT
  , evenUp, attrF, oddWS, oddTxPos, oddTxNoPos, nonTerminalEven, nonTerminalOdd
  , datatypeDT, tokenDT, nonTerminalE
  , uattrF
  , elementPosMixed, elementMixed, elementDT
  , naturalZeroDT, naturalOneDT, langDT, ncNameDT, stringTokenDT
  )
import Q4C12.XMLDesc.RApplicative as Export
  ( rfmap
  , RPlus (rplus, rempty)
  , RPlusApplyR (ActionR, rconsR), rright
  , RAlternative (rnil, rmany, rsome), rcons
  , roptional
  )
import Q4C12.XMLDesc.TH as Export
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
    g = unfoldr' unconsOddA

oddWSDropComments
  :: (Desc tag)
  => OddFlow tag ()
oddWSDropComments =
  rfmap (iso (const ()) (const mempty)) oddWS

flowWSEDropComments
  :: (Desc tag)
  => El tag a
  -> EvenFlow tag a
flowWSEDropComments
  = rfmap (dropUnit . from singleProd)
  . evenUp oddWSDropComments
  . rfmap singleProd

flowEvenPreWSDropComments
  :: (Desc tag)
  => EvenFlow tag a
  -> OddFlow tag a
flowEvenPreWSDropComments p
  = rfmap (from singleProd)
  $ rconsR p
  $ rfmap unitProd oddWSDropComments
