---
title: "Using Q4C12.XML.Desc: An Example"
...

Have you ever written a printer and a parser, and thought that the code from one could be used to define the other? Well, you can! This package also allows you to generate a [RELAX NG] schema from the same single specification.

In this example, we will build one phase of a small document processing system. We will accept input that has references to [Digital Object Identifiers][DOI] (DOIs) in it without title, date or author information, and outputs full references. A second phase, which is not implemented in this example, would only accept references with filled-in metadata and then build a HTML document from that input.

This document is Literate Haskell and can be run as-is with `runhaskell using-desc.lhs` (or, if you're reading a HTML render, copy-and-paste the contents and then run with `runhaskell`).

[DOI]: https://en.wikipedia.org/wiki/Digital_object_identifier
[RELAX NG]: https://en.wikipedia.org/wiki/RELAX_NG

Preliminaries
=============

First, the usual preliminaries of imports and `LANGUAGE` pragmata.

> {-# LANGUAGE DataKinds, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
> module Main where
> import Data.DList.NonEmpty (NonEmptyDList)
> import qualified Data.DList.NonEmpty as NEDList
> import Data.Either.Validation (Validation (Failure))
> import Data.Text.Lazy (Text)
> import Q4C12.TwoFinger (TwoFingerOddA)
> import Q4C12.XML ()
> import Q4C12.XML.Desc (Desc, El, OddFlow, rcons, rnil, rmany, elementMixed, makeRPlus, pat)
> import Test.Tasty.HUnit (assertBool)

The abstract syntax tree
========================

We will use GADTs to represent our AST to enforce the phase restriction on DOIs, which will also support sections, paragraphs, unordered lists, and italic and bold.

> data DOIResolution = DOIsResolved | DOIsUnresolved
>
> data Reference = Reference
>   { referenceTitle :: Text
>   , referenceDate :: Text
>   , referenceURL :: Text
>   , referenceAuthors :: [Text]
>   , referenceIdentifiers :: [Text]
>   }
> 
> data InlineElement :: DOIResolution -> * where
>   InlineDOI :: Text -> InlineElement 'DOIsUnresolved
>   InlineReference :: Reference -> InlineElement doiResolution
>   InlineItalic :: Inline doiResolution -> InlineElement doiResolution
>   InlineBold :: Inline doiResolution -> InlineElement doiResolution

Notice the difference between the types of `InlineDOIReference` and `InlineReference`: `InlineDOIReference` requires its type argument to be `'DOIsUnresolved`, but `InlineReference` can accept either resolved or unresolved.

> newtype Inline doiResolution = Inline { getInline :: TwoFingerOddA (InlineElement doiResolution) Text }

`TwoFingerOddA` is from [q4c12-twofinger]. It's isomorphic to `([(Text, InlineElement doiResolution)], Text)`, and it has a useful `Monoid` instance which we don't use in this simple example, but which is very handy for more complicated document processing.

[q4c12-twofinger]: https://hackage.haskell.org/package/q4c12-twofinger

> data BlockElement :: DOIResolution -> * where
>   BlockParagraph :: Inline doiResolution -> BlockElement doiResolution
>   BlockSection :: Text -> Block doiResolution -> BlockElement doiResolution
>   BlockUnorderedList :: [Block doiResolution] -> BlockElement doiResolution
>
> newtype Block doiResolution = Block { getBlock :: [BlockElement doiResolution] }

Write a parser, get a printer and schema for free
=================================================

Now for the fun part: defining the combined printer-parser-schema!

> blockDesc :: (Desc tag) => SingDOIResolution doiResolution -> OddFlow tag (Block doiResolution)
> blockDesc doiResolution = rfmap (iso Block getBlock) $ nonTerminalOdd productionName
>   flowEvenPreWS $ rmany $ flowWSE $ blockElementDesc doiResolution
>   where
>     productionName :: Text
>     productionName = case doiResolution of
>       SingDOIsUnresolved -> "block-unresolved"
>       SingDOIsResolved -> "block-resolved"

TODO something about even/odd flows here, note that this is ignoring whitespace, and nonTerminalOdd and injectivity requirements there (also how RELAX NG needs an element in a definition cycle)

> blockElementDesc :: (Desc tag) => SingDOIResolution doiResolution -> El tag (BlockElement doiResolution)
> blockElementDesc doiResolution =
>   $(makeRPlus $ do
>       pat [e| rfmap singleProd $ elementMixed (uname "p") $ inlineFlow doiResolution |]
>         $ \inlineP -> [p| BlockParagraph $inlineP |]
>       pat [e| elementMixed (uname "section") $ flowEvenPreWS
>                 $ rcons (elementMixed (uname "title") oddTx)
>                 $ rcons (inlineFlow doiResolution)
>                 $ rnil
>             |]
>         $ \titleP bodyP -> [p| BlockSection $titleP $bodyP |]
>       pat [e| rfmap singleProd $ elementMixed (uname "ul") $
>                 flowEvenPreWS $ rmany $ flowWSE $ elementMixed (uname "li") $
>                   blockDesc doiResolution
>             |]
>         $ \itemsP -> [p| BlockUnorderedList $itemsP |]
>    )

TODO apologise for the TH, explain what's going on there, mention that this will get good warnings out of GHC if a case is uncovered or duplicated (can we actually test that??)---mention what it *can't* do (e.g., can't pick up non-injectivity in the AST->XML direction; round-trip tests help there though)

> inlineDesc :: (Desc tag) => SingDOIResolution doiResolution -> OddFlow tag (Inline doiResolution)
> inlineDesc doiResolution = rfmap (iso Inline getInline) $ nonTerminalOdd productionName $
>   interleave (inlineElementDesc doiResolution) oddTx
>   where
>     productionName :: Text
>     productionName = case doiResolution of
>       SingDOIsUnresolved -> "inline-unresolved"
>       SingDOIsResolved -> "inline-resolved"
>
> inlineElementDesc :: (Desc tag) => SingDOIResolution doiResolution -> El tag (InlineElement doiResolution)
> inlineElementDesc doiResolution =
>   $(makeRPlus $ do
>       case doiResolution of
>         SingDOIsResolved -> return ()
>         SingDOIsUnresolved ->
>           pat [e| rfmap singleProd $ elementMixed (uname "doi") oddTx |]
>             $ \identP -> [p| InlineDOI $identP |]

TODO something about singletons? Definitely call attention to the case there.

>       pat [e| rfmap quintupleProd $ elementMixed (uname "ref")
>                 $ rcons (uattrF "title" stringTokenDT)
>                 $ rcons (uattrF "date" dateDT)
>                 $ rcons (uattrF "href" urlDT)
>                 $ rfmap collateReferenceFields $
>                     flowEvenPreWS $ rmany $ flowWSE referenceFieldDesc
>             |]
>         $ \titleP dateP hrefP authorsP identsP -> [p| InlineReference (Reference $titleP $dateP $hrefP $authorsP $identsP) |]

TODO datatypes, talk about isomorphisms and quotients

>       pat [e| rfmap singleProd $ elementMixed (uname "i") $ inlineDesc doiResolution |]
>         $ \inlineP -> [p| InlineItalic $inlineP |]
>       pat [e| rfmap singleProd $ elementMixed (uname "b") $ inlineDesc doiResolution |]
>         $ \inlineP -> [p| InlineBold $inlineP |]
>    )

Whew. And now all that's left is to put the whole thing together at the top level.

> documentDesc :: (Desc tag) => SingDOIResolution doiResolution -> El tag (Block doiResolution)
> documentDesc doiResolution = elementMixed "document" $ blockDesc doiResolution

DOI resolution
==============

This is not particularly enlightening: it's just walking the AST.

> lookupDOI :: Text -> Maybe Reference
> lookupDOI "10.17487/RFC2324" = Just $ Reference
>   { referenceTitle = "Hyper Text Coffee Pot Control Protocol (HTCPCP/1.0)"
>   , referenceDate = "1998-03"
>   , referenceURL = "https://tools.ietf.org/html/rfc2324"
>   , referenceAuthors = ["L. Masinter"]
>   , referenceIdentifiers = ["RFC 2324", "doi:10.17487/RFC2324"]
>   }
> lookupDOI _ = Nothing

For this example, we are only mocking the metadata retrieval: in a real application this would hit the network and query [CrossRef]'s API.

[CrossRef]: https://github.com/CrossRef/rest-api-doc

> data DOIResolutionError = UnknownDOI Text
>   deriving (Eq)
> 
> type Resolution = Validation (NonEmptyDList DOIResolutionError)
> 
> resolutionFailure :: DOIResolutionError -> Resolution a
> resolutionFailure = Failure . NEDList.singleton
>
> resolveDOIsInlineElement :: InlineElement 'DOIsUnresolved -> Resolution (InlineElement 'DOIsResolved)
> resolveDOIsInlineElement (InlineDOI doi) = case lookupDOI doi of
>   Nothing -> resolutionFailure $ UnknownDOI doi
>   Just reference -> pure $ InlineReference reference
> resolveDOIsInlineElement (InlineReference reference) = pure $ InlineReference reference
> resolveDOIsInlineElement (InlineItalic inline) = InlineItalic <$> resolveDOIsInline inline
> resolveDOIsInlineElement (InlineBold inline) = InlineBold <$> resolveDOIsInline inline
>
> resolveDOIsInline :: Inline 'DOIsUnresolved -> Resolution (InlineElement 'DOIsResolved)
> resolveDOIsInline = fmap Inline . bitraverse resolveDOIsInlineElement pure . getInline
>
> resolveDOIsBlockElement :: BlockElement 'DOIsUnresolved -> Resolution (BlockElement 'DOIsResolved)
> resolveDOIsBlockElement (BlockParagraph inline) = BlockParagraph <$> resolveDOIsInline inline
> resolveDOIsBlockElement (BlockSection title body) = BlockSection title <$> resolveDOIsBlock body
> resolveDOIsBlockElement (BlockUnorderedList items) = BlockUnorderedList <$> traverse resolveDOIsBlock items
>
> resolveDOIsBlock :: Block 'DOIsUnresolved -> Resolution (Block 'DOIsResolved)
> resolveDOIsBlock = fmap Block . traverse resolveDOIsBlockElement . getBlock

Note that the types here mean we can never forget to resolve a DOI: after this pass, it's statically guaranteed that there are no DOIs left!

Putting it all together
=======================

Example 1: Resolving a DOI and dumping to XML
---------------------------------------------

> example1 :: Bool
> example1 = first toNonEmpty rendered == Right (pure expected)
>   where
>     input :: Element ()
>     input = [xmlqq| <document><p><doi>10.17487/RFC2324</doi></p></document> |]
>     parsed :: Either Text (Block 'DOIUnresolved)
>     parsed = parse (documentDesc SignDOIsUnresolved) input
>     resolved :: Either Text (Resolution (Block 'DOIResolved))
>     resolved = resolveDOIs <$> parsed
>     rendered :: Either Text (Resolution (Element ()))
>     rendered = fmap (fmap $ render $ documentDesc SignDOIsResolved) resolved
>     expected :: Element ()
>     expected =
>       [xmlqq| <document><p><ref date="1998-03"
>                 href="https://tools.ietf.org/html/rfc2324"
>                 name="Hyper Text Coffee Pot Control Protocol (HTCPCP/1.0)"
>                 ><author>L. Masinter</author><ident>RFC 2324</ident><ident>doi:10.17487/RFC2324</ident></ref></document>
>             |]

Example 2: Failing to parse because of an unknown element
---------------------------------------------------------

> example2 :: Bool
> example2 = isLeft parsed
>   where
>     input :: Element ()
>     input = [xmlqq| <document><p><marquee>Whee!</marquee></p></document> |]
>     parsed :: Either Text (Block 'DOIUnresolved)
>     parsed = parse (documentDesc SignDOIsUnresolved) input

Example 3: Failing to parse because there is an unresolved DOI
--------------------------------------------------------------

> example3 :: Bool
> example3 = isLeft parsed
>   where
>     input :: Element ()
>     input = [xmlqq| <document><p><doi>10.17487/RFC2324</doi></p></document> |]
>     parsed :: Either Text (Block 'DOIResolved)
>     parsed = parse (documentDesc SignDOIsResolved) input

Testing all the examples
------------------------

> main :: IO ()
> main = do
>   assertBool "Example 1" example1
>   assertBool "Example 2" example2
>   assertBool "Example 3" example3

Some unrelated closing remarks
==============================

XML is not very fashionable these days. Considering some of the crazy things that were stuffed into angle brackets, I'm not sure I can blame anyone for reacting that way and retreating to JSON to retain their sanity, but I think we need to be careful not to throw the baby out with the bathwater.

In my opinion, XML is still the best meta-format for *documents* out there (as opposed to being a meta-format for structured data), and RELAX NG is a very well-thought-out piece of technology that should appeal to any functional programmer. I can recommend the [RELAX NG tutorial] as a quick introduction and ['RELAX NG' by Erik van der Vliste][RELAX NG book] for further reading.

[nxml-mode], which can use RELAX NG schemas, is quite brilliant---unsurprisingly, since James Clark worked on both. If you're working with documents and you're able to avoid XML's rough edges, this is an excellent choice. `Q4C12.XML.Desc.Schema` is the module that will take a description of an element and turn it into a RELAX NG schema. This will be in the XML syntax; [trang] (another James Clark creation) can convert from that to the compact syntax needed by nxml-mode. Try it on this example! It comes out quite startlingly readable, I think.

What if you still hate XML, or your data fits better into JSON? Most of the ideas in `Q4C12.XML.Desc` can be applied just as well to parsing/printing JSON; some bits would probably work even better (e.g., no need to be so fussy about whitespace or namespaces). [JSON Schema] is not nearly as nice as RELAX NG, and, to my knowledge, it doesn't have any good editor integrations.

[JSON schema]: http://json-schema.org/
[nxml-mode]: https://www.gnu.org/software/emacs/manual/html_mono/nxml-mode.html
[RELAX NG book]: http://books.xmlschemata.org/relaxng/page2.html
[RELAX NG tutorial]: http://relaxng.org/compact-tutorial-20030326.html
[trang]: https://github.com/relaxng/jing-trang
