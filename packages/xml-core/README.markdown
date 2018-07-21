`q4c12-xml-core` is an opinionated XML parser and renderer. It supports a subset of the intersection of XML 1.0 and Namespaces in XML 1.0, and, what it does support, it tries to support fully. Most of what is not supported is not-really-used-and-not-really-elsewhere-supported stuff, and some stray footguns.

It preserves comments and also provides location information in its parsed results, allowing for (almost) lossless source-to-source transformations and accurate error location reporting.

See also: the `q4c12-xml-desc` package. Write a parser, get a printer and a schema for free!

Limited XML profile
===================

Everything to do with DTDs is unsupported. No entity references aside from the ones specified in the XML standard, no default attributes, no external unparsed entities. No DOCTYPEs.

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

Streaming. The parser and renderer could be adjusted without fundamental difficulty, but I don't know how to fit `q4c12-xml-desc` into a streaming framework so I haven't bothered yet.
