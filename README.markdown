Mega-repo for my public Haskell projects.

Data structures & related utilities
===================================

* [q4c12-foldable-utils](packages/foldable-utils) are some utility functions I can't live without.

* [q4c12-hlist](packages/hlist) has heterogeneous lists and sums.

* [q4c12-mappend](packages/mappend) is `Data.Map`, but with the alternative append-ish `Monoid` instance. (Hence the name: map-pend.)

* [q4c12-position](packages/position) is a type to represent points or contiguous ranges in text.

* [q4c12-twofinger](packages/twofinger) is an efficient alternating sequence type.

Cabal stuff
===========

* [q4c12-aeson-cabal](packages/aeson-cabal) is my locally canonical place for orphan instances of classes from `aeson` for types from `Cabal`.

* [q4c12-defrost](packages/defrost) is like Stack's [`pvp-bounds` feature](https://docs.haskellstack.org/en/stable/yaml_configuration/#pvp-bounds), except it works on Cabal's freeze files instead. If you test your code against a set of freeze files, `defrost` can add the PVP bounds to your `.cabal` file corresponding to the dependency versions in those freeze files.

* [q4c12-project-file](packages/project-file) is a horribly incomplete parser and renderer for Cabal's [`.project` configuration files](http://cabal.readthedocs.io/en/latest/nix-local-build.html#configuring-builds-with-cabal-project).

XML
===

* [q4c12-xhtml2html](packages/xhtml2html) converts from the XML syntax to the HTML syntax.

* [q4c12-xml-core](packages/xml-core) is an opinionated XML parser/renderer that tries to stick to the spec where it's sensible and to drop things that aren't sensible.

* [q4c12-xml-desc](packages/xml-desc) uses q4c12-xml-core to build a RELAX NG schema generator. Write a parser, and get a renderer and schema for no extra effort!

Miscellanea
===========

* [q4c12-meta](packages/meta) is local housekeeping. It generates the `.travis.yml` script for CI and the various GHC-version-specific `.project` files under `cabal/`, as well as tending to the freeze files. It's working well for me so far, so maybe I should try to turn it into a generally useful thing.

* [q4c12-prelude](packages/prelude) is that hippest thing, an alternative `Prelude`. Its module is actually called `Prelude`; use it with `base-noprelude`, or with `mixins: base hiding (Prelude)` alongside `build-depends: base`.

  It's not very comprehensive: I'm adding things as I find I need them.

* [q4c12-romnum](packages/romnum) is a Roman numeral generator.

License
=======

The contents of this repository are dual-licensed under either the [Apache license, version 2.0](LICENSE.APACHE-2.0), or the [GNU Lesser General Public License, version 2.1 or later](LICENSE.LGPL-2.1), at the recepient's choice.
