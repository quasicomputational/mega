xhtml2html
==========

`xhtml2html` is a utility that does what it says: it takes HTML written in the XML syntax (specifically, in the limited profile of XML defined by `q4c12-xml-desc`) and outputs HTML written in the HTML syntax.

It does not know about XHTML's entities. Pre-process the input to replace them with their equivalent text.

Why?
----

Because the XML syntax can be a *lot* more verbose than the HTML syntax, especially with dumb encoders (like mine). Specifically, `q4c12-xml-desc` will generate XML that looks something like this:

  <n1:html xmlns:n1="http://www.w3.org/1999/xhtml">
    <n1:head xmlns:n1="http://www.w3.org/1999/xhtml">
      <n1:title xmlns:n1="http://www.w3.org/1999/xhtml">Hello, world!</n1:title>
  ...

This is perfectly cromulent and it makes the encoder's job a lot simpler. However, it's obviously very wordy.

Compression helps, but it's not quite a silver bullet. `gzip`'s maximum window size of 32 KiB also means that, for moderate to large documents, any extra repetetive verbiage is taking away space in the dictionary from more interesting things.
