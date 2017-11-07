{-# LANGUAGE TemplateHaskell #-}
module Q4C12.XML.Entity
  ( htmlResolver
  , htmlEntities
  , entitySetSchema
  , generateDTD
  )
  where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
import Formatting (bprint, builder, stext, hex)

import Q4C12.XML (DoctypeResolver, publicResolver)
import Q4C12.XML.TH (embedEntityMap, entitySetSchema)

htmlEntities :: Map SText LText
htmlEntities = $$(embedEntityMap "data/html.xml")
{-# NOINLINE htmlEntities #-}

htmlResolver :: DoctypeResolver
htmlResolver = publicResolver $
  Map.fromSet (const $ flip Map.lookup htmlEntities) magicHTMLPublicIds

-- From https://html.spec.whatwg.org/multipage/xhtml.html#parsing-xhtml-documents
magicHTMLPublicIds :: Set SText
magicHTMLPublicIds = Set.fromList
  [ "-//W3C//DTD XHTML 1.0 Transitional//EN"
  , "-//W3C//DTD XHTML 1.1//EN"
  , "-//W3C//DTD XHTML 1.0 Strict//EN"
  , "-//W3C//DTD XHTML 1.0 Frameset//EN"
  , "-//W3C//DTD XHTML Basic 1.0//EN"
  , "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN"
  , "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN"
  , "-//W3C//DTD MathML 2.0//EN"
  , "-//WAPFORUM//DTD XHTML Mobile 1.0//EN"
  ]

generateDTD :: [(SText, LText)] -> TBuilder
generateDTD = foldMap (\(name, val) -> generateEntityDecl name val <> "\n")

generateEntityDecl :: SText -> LText -> TBuilder
generateEntityDecl name val = bprint ("<!ENTITY " . stext . " \"" . builder . "\">") name (dtdEscape val)

--XML entities get their character references resolved once at their definition... and then again when they're referenced in the document. So, we escape twice, to catch all the tricky characters. We character-escape everything because that's easier (we wouldn't just have to do < and ", but also CR, LF and NL, because attribute normalisation works differently on character references and on literal text).
dtdEscape :: LText -> TBuilder
dtdEscape = foldMap (bprint ("&#x26;#x" . hex . ";") . Char.ord) . LT.unpack
