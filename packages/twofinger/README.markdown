This package provides efficient alternating sequences based on finger trees. These can represent sequences made up of two types of element, `e` and `a`  where two of the same type of element cannot follow each other directly.

Four different flavours are provided, isomorphic to `([(a, e)], a)`, `([(e, a)], e)`, `[(a, e)]`, and `[(e, a)]`.

Cons-like operations are *O(1)* amortised, and append operations are *O(log(min(n, m)))* amortised.

For more details, please see the Haddock documentation of Q4C12.TwoFinger.
