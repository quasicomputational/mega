module Q4C12.XML.XHTML2HTML
  ( htmlDocument
  , HTMLException
  , displayHTMLExceptionPos
  , displayHTMLException
  )
  where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Formatting (bprint, stext)
import Q4C12.Position (PositionRange, positionRange)
import Q4C12.TwoFinger (unconsOddA)

import Q4C12.XML (Element (Element), Markup (Markup), QName (QName), htmlNS, xmlNS, mathmlNS, svgNS, markupNull, elementPosition, hname)

--TODO: tag omission, especially </p>. Actually, no: if you want that, you can run a HTML minifier afterwards. Alternative TODO: remove the attribute value inspection and leave that to the minifier too.

data HTMLException pos
  = UnknownNamespace pos SText
  | MissingNamespace pos
  | ElementShouldBeVoid pos
  --TODO: list the containing <style> element's position, too?
  | LTInStyle pos
  | NBSPInStyle pos
  | ElementInStyle pos

displayHTMLExceptionPos :: HTMLException PositionRange -> TBuilder
displayHTMLExceptionPos (UnknownNamespace pos ns) = bprint
  ("Error while HTMLifying: Unknown namespace " . stext . " at " . positionRange . ".")
  ns pos
displayHTMLExceptionPos (MissingNamespace pos) = bprint
  ("Error while HTMLifying: Element in no namespace at " . positionRange . ".")
  pos
displayHTMLExceptionPos (ElementShouldBeVoid pos) = bprint
  ("Error while HTMLifying: Element at " . positionRange . " should be void.")
  pos
displayHTMLExceptionPos (LTInStyle pos) = bprint
  ("Error while HTMLifying: <style> can't have &lt; in it in the HTML syntax, but there's one at " . positionRange . ".")
  pos
displayHTMLExceptionPos (NBSPInStyle pos) = bprint
  ("Error while HTMLifying: unwise for <style> to have an NBSP in it in the HTML syntax, but there's one at " . positionRange . ".")
  pos
displayHTMLExceptionPos (ElementInStyle pos) = bprint
  ("Error while HTMLifying: <style> can't have child elements, but there's one at " . positionRange . ".")
  pos

displayHTMLException :: HTMLException pos -> TBuilder
displayHTMLException (UnknownNamespace _ ns) = bprint
  ("Error while HTMLifying: Unknown namespace " . stext . ".")
  ns
displayHTMLException (MissingNamespace _) =
  "Error while HTMLifying: Element in no namespace."
displayHTMLException (ElementShouldBeVoid _) =
  "Error while HTMLifying: Element should be void."
displayHTMLException (LTInStyle _) =
  "Error while HTMLifying: <style> can't have &lt; in it in the HTML syntax."
displayHTMLException (NBSPInStyle _) =
  "Error while HTMLifying: unwise for <style> to have an NBSP in it in the HTML syntax."
displayHTMLException (ElementInStyle _) = bprint
  "Error while HTMLifying: <style> can't have child elements."

htmlDocument :: Element pos -> Either (HTMLException pos) TBuilder
htmlDocument el = foldSequence
  [ pure "<!DOCTYPE html>"
  , htmlElement el
  ]

blessedNamespaces :: Set SText
blessedNamespaces = Set.fromList [htmlNS, svgNS, mathmlNS]

--TODO: <style> isn't the only element with stupid escaping behaviour; see https://html.spec.whatwg.org/#parsing-html-fragments for others.
htmlElement :: Element pos -> Either (HTMLException pos) TBuilder
htmlElement (Element qn attrs body pos) = foldSequence
  [ pure "<"
  , case qn of
      QName Nothing _ -> Left $ MissingNamespace pos
      QName (Just ns) local -> do
        unless (Set.member ns blessedNamespaces) $
          Left $ UnknownNamespace pos ns
        pure $ LTB.fromText local
  , flip foldMapM (Map.toList attrs) $ \(an, (attrPos, parts)) -> foldSequence $
    let val = foldMap snd parts
    in [ pure " "
       , case an of
           QName Nothing local -> pure $ LTB.fromText local
           QName (Just ns) local
             | ns == xmlNS && local == "lang" ->
                 pure "lang"
             | ns == xmlNS ->
                 pure $ "xml:" <> LTB.fromText local
             | otherwise ->
                 Left $ UnknownNamespace attrPos ns
       , pure $ if LT.null val
           then mempty
           else fold
             [ "="
             , if LT.all isSafeUnquoted val
               then LTB.fromLazyText $ escapeAttr val
               else fold ["\"", LTB.fromLazyText $ escapeAttr val, "\""]
             ]
       ]
  , pure ">"
  , if elementShouldBeVoid qn
    then if markupNull body
         then pure mempty
         else Left $ ElementShouldBeVoid pos
    else foldSequence
      [ htmlMarkup (qn == hname "style") body
      , pure "</"
      , case qn of
          QName Nothing _ -> Left $ MissingNamespace pos
          QName (Just ns) local -> do
            unless (Set.member ns blessedNamespaces) $
              Left $ UnknownNamespace pos ns
            pure $ LTB.fromText local
      , pure ">"
      ]
  ]

htmlMarkup :: Bool -> Markup pos -> Either (HTMLException pos) TBuilder
htmlMarkup False (Markup inl) = bifoldMapM htmlElement (pure . LTB.fromLazyText . escapeText . foldMap snd) inl
htmlMarkup True (Markup inl) = case unconsOddA inl of
  Right ((_, el), _) -> Left $ ElementInStyle $ elementPosition el
  Left chunks -> flip foldMapM chunks $ \case
    (pos, t)
      | LT.any (== '<') t -> Left $ LTInStyle pos
      | LT.any (== '\xA0') t -> Left $ NBSPInStyle pos
      | otherwise -> Right $ LTB.fromLazyText t

escapeText :: LText -> LText
escapeText = LT.replace "<" "&lt;" . LT.replace ">" "&gt;" . LT.replace "\xA0" "&nbsp;" . LT.replace "&" "&amp;"

escapeAttr :: LText -> LText
escapeAttr = LT.replace "\"" "&quot;" . escapeText

elementShouldBeVoid :: QName -> Bool
elementShouldBeVoid (QName nsMay local) =
  nsMay == Just htmlNS && Set.member local voidHTMLElements
  where
    voidHTMLElements :: Set SText
    voidHTMLElements = Set.fromList
      [ "area"
      , "base"
      , "basefont"
      , "bgsound"
      , "br"
      , "col"
      , "embed"
      , "frame"
      , "hr"
      , "img"
      , "input"
      , "keygen"
      , "link"
      , "meta"
      , "param"
      , "source"
      , "track"
      , "wbr"
      ]

isSafeUnquoted :: Char -> Bool
isSafeUnquoted c = isAlphaNum c || c == '-' || c == '_'
