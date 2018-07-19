Write a parser, get a printer and a schema for free
===================================================

`Q4C12.XMLDesc` and its submodules provides a way to describe the structure of an XML document and how to map it to and from a Haskell value. A RELAX NG schema can then be generated from the description. Custom datatype libraries (in the RELAX NG sense of the word) are supported.

There is one minor footgun: if your schema is recursive, then you must use one of the non-terminal functions to give a part a name and to break the cycle. Otherwise, it works fine as a parser and a printer, but a RELAX NG schema cannot be infinite.

Key ideas and debts owed
------------------------

`Q4C12.XMLDesc` uses an `Applicative`-like approach, except instead of having `fmap :: (a -> b) -> f a -> f b`, it has `rfmap :: Iso' a b -> f a -> f b`, where `Iso'` is a (full) isomorphism: it can go in either direction losslessly. (Note that 'lossless' may be context-dependent: for example, ignoring space at the start and end of a field is not an isomorphism over all strings, but it may be suitable for a specific application.) This is similar to the idea of using *partial* isomorphisms (outlined in, e.g., [this blog post](http://ponies.io/posts/2015-03-01-round-tripping-balls-json-1.html) and originally described in [this 2010 paper by Tillman Rendel and Klaus Ostermann](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf)), but without the partiality.

In addition, the van Laarhoven representation of isomorphisms is used, allowing interopability with the `lens` library, and reducing the need for Template Haskell.

Using the `Desc` typeclass for an abstract structure definition and then having separate print, parse and generate-the-schema instances is outlined by [Joachim Breitner in a blog post](http://www.joachim-breitner.de/blog/710-Showcasing_Applicative); it also owes an obvious debt to the inimitable Oleg and his work on [tagless final interpreters](http://okmij.org/ftp/tagless-final/index.html).
