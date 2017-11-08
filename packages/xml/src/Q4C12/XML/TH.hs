{-# LANGUAGE TemplateHaskellQuotes #-}
module Q4C12.XML.TH
  ( embedEntityMap
  , entitySetSchema
  )
  where

import qualified Data.Map as Map
import Data.String (String)
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import Language.Haskell.TH (Q, TExp, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)

import Q4C12.XML (uname, parseXML, noEntities)
import Q4C12.XML.Desc
  ( rfmap, rconsR, rmany, Desc, El, elementMixed, flowEvenPreWS
  , flowWSE, uattrF, ncNameDT, oddTx
  )
import Q4C12.XML.Desc.Parse (parse)

--TODO: this ought to be in an Internal module
entitySetSchema :: (Desc tag) => El tag [(SText, LText)]
entitySetSchema = elementMixed (uname "entity-set") $ flowEvenPreWS $
  rmany $ flowWSE $ rfmap (from doubleProd) $ elementMixed (uname "entity")
    $ rconsR (rfmap (iso LT.toStrict LT.fromStrict) $ uattrF "name" ncNameDT)
    $ rfmap singleProd oddTx

embedEntityMap :: FilePath -> Q (TExp (Map SText LText))
embedEntityMap fileName = do
  --TODO: why is Q not an instance of MonadIO?
  (parseRes, _) <- runIO $ parseXML noEntities <$> STIO.readFile fileName
  addDependentFile fileName
  --TODO: more informative errors? Avoid throwing away warnings?
  --TODO: warn for bogus entity names and duplicates? Oh, no need to warn for bogus names (once we get proper checking in ncNameDT, anyway).
  case parseRes of
    Left _ -> fail "XML parsing failed."
    Right xml -> case parse entitySetSchema xml of
      Nothing -> fail "Could not read data."
      --Ugly hack because Text doesn't have a Lift instance, and nor does Map
      Just d -> [e|| Map.fromList (bimap ST.pack LT.pack <$> pairs) ||]
        where
          pairs :: [(String, String)]
          pairs = bimap ST.unpack LT.unpack <$> d
