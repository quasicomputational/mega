module Q4C12.XML
  ( Element (Element), element, elementPosition
  , QName (QName)
  , Markup (Markup, getMarkup), markupNull
  , markupElement, markupText, markupSText, markupBuilder
  , Content (Content, getContent), contentNull
  , contentText, contentComment
  , Comment (Comment, getComment)
  , htmlNS, xmlNS, svgNS, mathmlNS
  , hname, xname, rngname, uname, rngelem, helem
  , addUAttr, addUAttrS, addAttr, addAttrS, addAttrs
  , XMLError, displayError
  , XMLWarning, displayWarnings, displayWarning
  , isXMLSpace
  , Parsed
  , rootElement, preRootComments, postRootComments
  , parseXML, parseXMLCommented
  , renderXML
  , parseXML'
  )
  where

import Q4C12.Position (PositionRange)

--TODO: can we fix this dumb duplication with the module export trick?
import Q4C12.XML.Internal
  ( Element (Element), element, elementPosition
  , QName (QName)
  , Markup (Markup, getMarkup), markupNull
  , markupElement, markupText, markupSText, markupBuilder
  , Content (Content, getContent), contentNull
  , contentText, contentComment
  , Comment (Comment, getComment)
  , htmlNS, xmlNS, svgNS, mathmlNS
  , hname, xname, rngname, uname, rngelem, helem
  , addUAttr, addUAttrS, addAttr, addAttrS, addAttrs
  , XMLError, displayError
  , XMLWarning, displayWarnings, displayWarning
  , isXMLSpace
  , Parsed
  , rootElement, preRootComments, postRootComments
  , parseXML, parseXMLCommented
  , renderXML
  )

parseXML'
  :: SText
  -> (Either XMLError (Element Void PositionRange), [XMLWarning])
parseXML' = parseXML
