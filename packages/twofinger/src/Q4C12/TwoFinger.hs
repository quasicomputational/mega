-- | This module provides alternating finger trees, which are similar
-- to "Data.Sequence" in the @containers@ package, or
-- "Data.FingerTree" in the @fingertree@ package, except that, between
-- every element (of type @e@) in the \'normal\' finger tree, there is
-- a \'separator\' of type @a@. @'TwoFingerOddA' e ()@ is isomorphic
-- to @[e]@, and @'TwoFingerOddA' e a@ is isomorphic to @([(a, e)], a)@.
-- (The type variables are in that order because that permits a
-- 'Traversable1' instance for 'TwoFingerOddA'.)
--
-- Four flavours of alternating finger trees are present,
-- corresponding to different element patterns:
--
--   * @'TwoFingerOddA' e a@, which is like @a (e a)*@.
--   * @'TwoFingerOddE' e a@, which is like @e (a e)*@.
--   * @'TwoFingerEvenA' e a@, which is like @(a e)*@.
--   * @'TwoFingerEvenE' e a@, which is like @(e a)*@.
--
-- The flavours' names first describe whether they have the same
-- number of @a@s and @e@s within them (the @Even@ flavours do, the
-- @Odd@ ones do not), and then whether the first element is an @e@ or
-- an @a@.
--
-- (Full) conses and snocs prepend or append a pair of elements to the
-- front or rear of an alternating finger tree, keeping the flavour
-- the same. Half-conses and -snocs transform between these flavours,
-- adding only half the pair. All cons-like operations have an inverse
-- operation. Some half-conses and -snocs and their inverses are
-- \(O(1)\) amortised, with \(O(\log n)\) worst case, while some are \(O(1)\)
-- always. All full conses, snocs and inverses are \(O(1)\) amortised
-- and \(O(\log n)\) worst case.
--
-- Note that the names of half-conses and -snocs take the flavour that
-- they operate on, which means that, for example, 'halfconsOddA' and
-- 'halfunconsOddA' are __not__ inverses; the actual inverse pairs are
-- 'halfconsOddA' + 'halfunconsEvenE' and 'halfconsEvenE' +
-- 'halfunconsOddA'.
--
-- Appending alternating finger trees is also efficient. As well as
-- the usual 'Monoid' and 'Semigroup' instances, the two @Even@
-- flavours can be viewed as monoid actions of the @Odd@ flavours. All
-- append-like operations are \(O(\log(\min(n, m)))\) amortised and
-- \(O(\log(\max(n, m)))\) worst case.
--
-- For more information on finger trees, see:
--
--    * Ralf Hinze and Ross Paterson,
--      \"Finger trees: a simple general-purpose data structure\",
--      /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--      <http://staff.city.ac.uk/~ross/papers/FingerTree.html>
--
-- This package's alternating finger trees are not annotated with
-- sizes as described in section 4 of the paper.
module Q4C12.TwoFinger
  ( -- * TwoFingerOddA
    TwoFingerOddA,
    -- ** Construction and analysis
    singletonOddA, unitOddA, onlyOddA, interleavingOddA,
    -- ** Full conses
    consOddA, unconsOddA, snocOddA, unsnocOddA,
    -- ** Half conses
    halfconsOddA, halfunconsOddA, halfsnocOddA, halfunsnocOddA,
    -- ** Lenses
    firstOddA, lastOddA,
    -- * TwoFingerOddE
    TwoFingerOddE,
    -- ** Construction
    singletonOddE,
    -- ** Full conses
    consOddE, snocOddE, unconsOddE, unsnocOddE,
    -- ** Half conses
    halfconsOddE, halfsnocOddE, halfunconsOddE, halfunsnocOddE,
    -- * TwoFingerEvenA
    TwoFingerEvenA,
    -- ** Full conses
    consEvenA, unconsEvenA, snocEvenA, unsnocEvenA,
    -- ** Half conses
    halfconsEvenA, halfsnocEvenA, halfunconsEvenA, halfunsnocEvenA,
    -- * TwoFingerEvenE
    TwoFingerEvenE,
    -- ** Full conses
    consEvenE, unconsEvenE, snocEvenE, unsnocEvenE,
    -- ** Half conses
    halfconsEvenE, halfsnocEvenE, halfunconsEvenE, halfunsnocEvenE,
    -- * Appending different flavours
    -- $monoid_action_properties
    -- ** Monoid actions
    appendEvenAOddA, appendOddEEvenA,
    appendOddAEvenE, appendEvenEOddE,
    -- ** Two odds make an even
    appendOddAOddE, appendOddEOddA,
    -- * Aligning (zipping)
    alignLeftOddAOddA, alignLeftOddAEvenA,
    alignLeftOddEOddE, alignLeftOddEEvenE,
  )
  where

import Q4C12.TwoFinger.Internal
  (
    TwoFingerOddA,
    singletonOddA, unitOddA, onlyOddA, interleavingOddA,
    consOddA, unconsOddA, snocOddA, unsnocOddA,
    halfconsOddA, halfunconsOddA, halfsnocOddA, halfunsnocOddA,
    firstOddA, lastOddA,
    TwoFingerOddE,
    singletonOddE,
    consOddE, snocOddE, unconsOddE, unsnocOddE,
    halfconsOddE, halfsnocOddE, halfunconsOddE, halfunsnocOddE,
    TwoFingerEvenA,
    consEvenA, unconsEvenA, snocEvenA, unsnocEvenA,
    halfconsEvenA, halfsnocEvenA, halfunconsEvenA, halfunsnocEvenA,
    TwoFingerEvenE,
    consEvenE, unconsEvenE, snocEvenE, unsnocEvenE,
    halfconsEvenE, halfsnocEvenE, halfunconsEvenE, halfunsnocEvenE,
    appendEvenAOddA, appendOddEEvenA,
    appendOddAEvenE, appendEvenEOddE,
    appendOddAOddE, appendOddEOddA,
    alignLeftOddAOddA, alignLeftOddAEvenA,
    alignLeftOddEOddE, alignLeftOddEEvenE,
  )

-- $setup
-- >>> import Q4C12.TwoFinger.Internal (AnyOddA (AnyOddA), AnyOddE (AnyOddE), AnyEvenA (AnyEvenA), AnyEvenE (AnyEvenE))
-- >>> import Data.Semigroup ((<>))

-- $monoid_action_properties
-- prop> \(AnyOddA a) (AnyOddA b) (AnyEvenE c) -> appendOddAEvenE (a <> b) c == a <> appendOddAEvenE b c
-- prop> \(AnyOddA a) (AnyOddE b) (AnyOddA c) -> appendEvenAOddA (appendOddAOddE a b) c == appendOddAEvenE a (appendOddEOddA b c)
-- prop> \(AnyOddA a) (AnyOddE b) (AnyEvenA c) -> appendOddAOddE a (appendOddEEvenA b c) == appendOddAOddE a b <> c
-- prop> \(AnyOddA a) (AnyEvenE b) (AnyOddE c) -> appendOddAOddE a (appendEvenEOddE b c) == appendOddAOddE (appendOddAEvenE a b) c
-- prop> \(AnyOddA a) (AnyEvenE b) (AnyEvenE c) -> appendOddAEvenE a (b <> c) == appendOddAEvenE (appendOddAEvenE a b) c
--
-- prop> \(AnyOddE a) (AnyOddA b) (AnyOddE c) -> appendOddEEvenA a (appendOddAOddE b c) == appendEvenEOddE (appendOddEOddA a b) c
-- prop> \(AnyOddE a) (AnyOddA b) (AnyEvenE c) -> appendOddEOddA a (appendOddAEvenE b c) == appendOddEOddA a b <> c 
-- prop> \(AnyOddE a) (AnyEvenA b) (AnyOddA c) -> appendOddEOddA a (appendEvenAOddA b c) == appendOddEOddA (appendOddEEvenA a b) c
-- prop> \(AnyOddE a) (AnyEvenA b) (AnyEvenA c) -> appendOddEEvenA a (b <> c) == appendOddEEvenA (appendOddEEvenA a b) c
--
-- prop> \(AnyEvenA a) (AnyOddA b) (AnyOddA c) -> appendEvenAOddA a (b <> c) == appendEvenAOddA a b <> c
-- prop> \(AnyEvenA a) (AnyOddA b) (AnyOddE c) -> appendOddAOddE (appendEvenAOddA a b) c == a <> appendOddAOddE b c
-- prop> \(AnyEvenA a) (AnyOddA b) (AnyEvenE c) -> appendOddAEvenE (appendEvenAOddA a b) c == appendEvenAOddA a (appendOddAEvenE b c)
-- prop> \(AnyEvenA a) (AnyEvenA b) (AnyOddA c) -> appendEvenAOddA (a <> b) c == appendEvenAOddA a (appendEvenAOddA b c)
--
-- prop> \(AnyEvenE a) (AnyOddE b) (AnyOddA c) -> appendOddEOddA (appendEvenEOddE a b) c == a <> appendOddEOddA b c
-- prop> \(AnyEvenE a) (AnyOddE b) (AnyEvenA c) -> appendOddEEvenA (appendEvenEOddE a b) c == appendEvenEOddE a (appendOddEEvenA b c)
-- prop> \(AnyEvenE a) (AnyEvenE b) (AnyOddE c) -> appendEvenEOddE (a <> b) c == appendEvenEOddE a (appendEvenEOddE b c)
