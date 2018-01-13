`q4c12-xml` is a collection of tools for working with XML. It supports a subset of the intersection of XML 1.0 and Namespaces in XML 1.0. Most of what is not supported is not-really-used-and-not-really-elsewhere-supported stuff, and some stray footguns.

Write a parser, get a printer and a schema for free
===================================================

`Q4C12.XML.Desc` and its submodules provides a way to describe the structure of an XML document and how to map it to and from a Haskell value. A RELAX NG schema can then be generated from the description. Custom datatype libraries (in the RELAX NG sense of the word) are supported.

There is one minor footgun: if your schema is recursive, then you must use one of the non-terminal functions to give a part a name and to break the cycle. Otherwise, it works fine as a parser and a printer, but a RELAX NG schema cannot be infinite.

Key ideas and debts owed
------------------------

`Q4C12.XML.Desc` uses an `Applicative`-like approach, except instead of having `fmap :: (a -> b) -> f a -> f b`, it has `rfmap :: Iso' a b -> f a -> f b`, where `Iso'` is a (full) isomorphism: it can go in either direction losslessly. (Note that 'lossless' may be context-dependent: for example, ignoring space at the start and end of a field is not an isomorphism over all strings, but it may be suitable for a specific application.) This is similar to the idea of using *partial* isomorphisms (outlined in, e.g., [this blog post](http://ponies.io/posts/2015-03-01-round-tripping-balls-json-1.html) and originally described in [this 2010 paper by Tillman Rendel and Klaus Ostermann](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf)), but without the partiality.

In addition, the van Laarhoven representation of isomorphisms is used, allowing interopability with the `lens` library, and reducing the need for Template Haskell.

Using the `Desc` typeclass for an abstract structure definition and then having separate print, parse and generate-the-schema instances is outlined by [Joachim Breitner in a blog post](http://www.joachim-breitner.de/blog/710-Showcasing_Applicative); it also owes an obvious debt to the inimitable Oleg and his work on [tagless final interpreters](http://okmij.org/ftp/tagless-final/index.html).

Limited XML profile
===================

Most things to do with DTDs are unsupported. No default attributes, no external unparsed entities. Internal subsets—even empty ones—are rejected.

Restricted entity references are permitted. These must not contain markup. The replacement text is treated as if it was entirely made up of character references; this matters for attribute value normalisation. The entity-set to be used is identified by the tuple of the PUBLIC identifier and the SYSTEM identifier in the DOCTYPE. Library users can define their own entity-sets. It is recommended that only the SYSTEM identifier is used for this purpose; however, the HTML specification specifies a similar mechanism switching on the PUBLIC identifier, so that is supported.

The XML declaration is forbidden.

Processing instructions are forbidden.

CDATA sections are forbidden.

Namespace prefixes are thrown away in parsing and are not retained in the internal data structures. Consequently, when rendering, new prefixes are generated; this is done in a canonical way, s.t. default namespaces and lexical scoping of namespaces are never used.

Entity references, attribute names, element names and namespaces must be in NFD.

End-of-line normalisation is performed. Attribute value normalisation is also performed, but discouraged. Rendering uses the Unix convention of LF for line-ends. For round-tripping purposes, CR is always rendered as a character reference, and tab and LF are additionally rendered as character references in attribute values.

`>` is forbidden from appearing as a literal; it must appear as a reference. This is because of the silly `]]>` special-case.

What's not in the profile
-------------------------

Most of what's left out can be dealt with by a syntactic transformation of the input. There are some intentionally unsupported use cases; if any of the following matter to you, I am afraid that you will need a different library.

* Proper DTD support beyond the restricted entity reference support.

* Mixed normalised and un-normalised entity references, element names, attribute names or namespaces, or mixed normalisation forms.

* Dependency upon namespace prefixes (e.g., CURIEs, RDFa 1.0).

* Entity references, attribute names and element names that do not match the productions in Namespaces in XML 1.0 (e.g., because they include multiple colons).

Other things it doesn't do
==========================

Streaming. The parser and renderer could be adjusted without fundamental difficulty, but I don't know how to fit `Q4C12.XML.Desc` into a streaming framework.

xhtml2html
==========

`xhtml2html` is a utility that does what it says: it takes HTML written in the XML syntax (specifically, in the limited profile of XML defined above) and outputs HTML written in the HTML syntax.

Why?
----

Because the XML syntax can be a *lot* more verbose than the HTML syntax, especially with dumb encoders (like mine). Specifically, `q4c12-xml` will generate XML that looks something like this:

  <n1:html xmlns:n1="http://www.w3.org/1999/xhtml">
    <n1:head xmlns:n1="http://www.w3.org/1999/xhtml">
      <n1:title xmlns:n1="http://www.w3.org/1999/xhtml">Hello, world!</n1:title>
  ...

This is perfectly cromulent and it makes the encoder's job a lot simpler. However, it's obviously very wordy.

Compression helps, but it's not quite a silver bullet. `gzip`'s maximum window size of 32 KiB also means that, for moderate to large documents, any extra repetetive verbiage is taking away space in the dictionary from more interesting things.
