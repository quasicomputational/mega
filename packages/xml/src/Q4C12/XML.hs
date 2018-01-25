module Q4C12.XML
  ( Element (Element), element, elementPosition
  , QName (QName)
  , DoctypeResolver, systemResolver, publicResolver, noEntities
  , Markup (Markup, getMarkup), markupNull
  , markupElement, markupText, markupSText, markupBuilder
  , htmlNS, xmlNS, svgNS, mathmlNS
  , hname, xname, rngname, uname, rngelem, helem
  , addUAttr, addUAttrS, addAttr, addAttrS, addAttrs
  , XMLError, displayError
  , XMLWarning, displayWarnings, displayWarning
  , isXMLSpace
  , parseXML
  , renderXML
  )
  where

import Q4C12.XML.Internal
  ( Element (Element), element, elementPosition
  , QName (QName)
  , DoctypeResolver, systemResolver, publicResolver, noEntities
  , Markup (Markup, getMarkup), markupNull
  , markupElement, markupText, markupSText, markupBuilder
  , htmlNS, xmlNS, svgNS, mathmlNS
  , hname, xname, rngname, uname, rngelem, helem
  , addUAttr, addUAttrS, addAttr, addAttrS, addAttrs
  , XMLError, displayError
  , XMLWarning, displayWarnings, displayWarning
  , isXMLSpace
  , parseXML
  , renderXML
  )
