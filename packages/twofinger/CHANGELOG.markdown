Next
====

* Allow `deepseq` 1.4.2.0 and `containers` 0.5.7.1, which are boot libraries for GHC 8.0.
* Added `SUPPORT.markdown`, documenting which GHCs this package will build on.
* Packaging: requiring `cabal-doctest` 1.0.5 cleans up some ugliness, and lets us DTRT.

q4c12-twofinger 0.2 (2018-01-17)
================================

* Fix a dangling section reference in the haddocks.
* Allow `tasty` 1.0 along with `tasty` 0.12.
* More tests!
* `Bitraversable1` and `Bifoldable1` for `TwoFingerOddE`.
* Re-use `Seq` from `containers` instead of rolling our own finger-trees. This changes some worst-case running time, some for the better and some for the worse.
* Drop `align*`, `infinite*` and `repeat*`. The `align*` functions did not work properly before in the infinite/infinite case, and `Seq` from `containers` does not support infinite trees; further, implementing aligning directly with `uncons` is often clearer.

q4c12-twofinger 0.1 (2018-01-04)
================================

* Packaging: list `CHANGELOG.markdown` and `README.markdown` as `extra-doc-files`, rather than `extra-source-files`.
* Drop the `QuickCheck` dependency from the library. Move the property tests out of haddocks and into the testsuite.

q4c12-twofinger 0.0.0.2 (2017-12-08)
====================================

* Loosened `bifunctors` dependency bounds.

q4c12-twofinger 0.0.0.1 (2017-12-08)
====================================

* Packaging fix-ups only.

q4c12-twofinger 0 (2017-12-08)
==============================

* Initial release.
