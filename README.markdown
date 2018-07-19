Mega-repo for my public Haskell projects.

In roughly decreasing order of likelihood of being useful to anyone else, they are:

* [q4c12-twofinger](packages/twofinger) is an efficient alternating sequence type.

* [q4c12-xml-desc](packages/xml-desc) uses q4c12-xml-core to build a RELAX NG schema generator. Write a parser, and get a renderer and schema for no extra effort!

* [q4c12-xml-core](packages/xml-core) is an opinionated XML parser/renderer that tries to stick to the spec where it's sensible and to drop things that aren't sensible.

* [q4c12-foldable-utils](packages/foldable-utils) are some utility functions I can't live without.

* [q4c12-mappend](packages/mappend) is `Data.Map`, but with the alternative append-ish `Monoid` instance. (Hence the name: map-pend.)

* [q4c12-xhtml2html](packages/xhtml2html) converts from the XML syntax to the HTML syntax.

* [q4c12-romnum](packages/romnum) is a Roman numeral generator.

* [q4c12-position](packages/position) is a type to represent points or contiguous ranges in text.

* [q4c12-hlist](packages/hlist) has heterogeneous lists and sums.

* [q4c12-prelude](packages/prelude) is that hippest thing, an alternative `Prelude`. Its module is actually called `Prelude`; use it with `base-noprelude`.

  It's not very comprehensive: I'm adding things as I find I need them.
