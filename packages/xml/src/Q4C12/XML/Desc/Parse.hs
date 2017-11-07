{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Q4C12.XML.Desc.Parse
  ( parse
  , Parse
  ) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT
import qualified Data.Sequence as Seq
import Q4C12.TwoFinger (halfsnocEvenA, halfunconsOddE, halfunconsOddE, halfsnocEvenE, halfunsnocEvenA, halfunconsOddA, halfunsnocOddA)

import Q4C12.XML (QName, Element (Element), Markup (Markup))
import Q4C12.XML.Desc.Class
  ( Desc ( DT, El, OddFlow, EvenFlow, Pos, nonTerminalE, elementE, tokenDT
         , nonTerminalOdd, oddTx, oddTxPos, oddWS, nonTerminalEven, attrF, evenUp
         , elementEPos, datatypeDT
         )
  , Datatype (Datatype)
  , CompleteFlow (CompleteFlowDT, CompleteFlowMixed)
  )
import Q4C12.XML.Desc.RApplicative
  ( RFunctor (rfmap), RPlusApplyR (ActionR, rconsR), RPlus (rempty, rplus)
  , RAlternative (rnil)
  )

data Parse pos

--TODO: visibility into token lists? Also, child counting for that and PathStepE
data PathStep = PathStepE QName | PathStepA QName
  deriving (Show, Eq)

runParseDTTotal :: DT (Parse pos) a -> LText -> Either LText a
runParseDTTotal dtp t = do
  --TODO: check that this is tokenising correctly, particularly wrt Unicode's many and varied space characters
  (a, leftover) <- runParseDT dtp (Seq.fromList $ LT.words t)
  unless (Seq.null leftover) $ Left "unconsumed tokens"
  pure a

--TODO: better errors, also line/column information.
--TODO: streaming parsing; this may require changes to the Desc class...
--TODO: unicode normalisation wherever we might be comparing strings
instance (Show pos) => Desc (Parse pos) where
  --TODO: these actually don't need to run on trees; we could directly operate on the xml-conduit event stream... We could have another Desc instance for that, though!
  data EvenFlow (Parse pos) a = ParsePF
    { runParsePF
        :: TwoFingerEvenA (Element pos) (Seq (pos, LText))
        -> Map QName LText
        -> Either ([PathStep], LText)
             (a, TwoFingerEvenA (Element pos) (Seq (pos, LText)), Map QName LText)
    }
    deriving (Functor)
  data OddFlow (Parse pos) a = ParseCF
    { runParseCF :: TwoFingerOddA (Element pos) (Seq (pos, LText))
                 -> Map QName LText
                 -> Either ([PathStep], LText)
                      (a, TwoFingerEvenE (Element pos) (Seq (pos, LText)), Map QName LText)
    }
    deriving (Functor)
  data El (Parse pos) a = ParseE
    { runParseE :: Element pos -> Either ([PathStep], LText) a }
    deriving (Functor)
  data DT (Parse pos) a = ParseDT
    { runParseDT :: Seq LText
                 -> Either LText (a, Seq LText)
    }
    deriving (Functor)
  type Pos (Parse pos) = pos

  evenUp op ep = ParsePF $ \rft attrs -> case halfunsnocEvenA rft of
    Nothing -> Left ([], "evenUp: no elements!")
    Just (ift, ez) -> do
      (a, lft, attrs') <- runParseCF op ift attrs
      let uft = halfsnocEvenE lft ez
          (e, rft') = halfunconsOddE uft
      as <- runParseE ep e
      pure (HProdCons a as, rft', attrs')

  --TODO: whitespace normalisation??
  attrF qname tp = ParsePF $ \flow attrs ->
    first (first $ (:) $ PathStepA qname) $
      case Map.splitLookup qname attrs of
        (_, Nothing, _) -> Left ([], "attrF no attr")
        (as, Just t, bs) -> case runParseDTTotal tp t of
          Left err -> Left ([], err)
          Right val -> Right (val, flow, as <> bs)

  --TODO: is isSpace correct here? What about nbsp? Must review the RELAX NG spec.
  oddWS = ParseCF $ \ilv attrs -> case halfunconsOddA ilv of
    (a, xs) -> do
      unless (all (LT.all isSpace . snd) a) $
        Left ([], "non-space in string that must be only whitespace")
      pure ((), xs, attrs)

  oddTxPos = ParseCF $ \ilv attrs -> case halfunconsOddA ilv of
    (a, xs) -> pure (a, xs, attrs)
  oddTx = foldMap snd <$> oddTxPos

  --TODO: use these for helpful error location reporting?
  nonTerminalEven _ = id
  nonTerminalOdd _ = id

  elementE name
    = fmap (view (tailL . from singleProd))
    . elementEPos name
    . rfmap singleProd

  elementEPos name (CompleteFlowMixed fp) = ParseE $
    \(Element qn attrs (Markup body) pos) -> do
      unless (qn == name) $
        Left ([PathStepE qn], "elementE non-matching name")
      first (first (PathStepE name :)) $ do
        (a, rest, attrs') <- runParseCF fp body $ foldMap snd . snd <$> attrs
        unless (null rest) $
          Left ([], LT.pack $ "elementE mixed: unconsumed contents: " <> show rest)
        unless (Map.null attrs') $
          Left ([], LT.pack $ "elementT mixed: unconsumed attrs: " <> show attrs')
        pure $ HProdCons pos a

  elementEPos name (CompleteFlowDT dtp) = ParseE $
    \(Element qn _ (Markup ilv) pos) -> do
      unless (qn == name) $
        Left ([PathStepE qn], "elementE non-matching name")
      first (first (PathStepE name :)) $ case halfunconsOddA ilv of
        (t, rest) -> do
          unless (null rest) $
            Left ([], "elementE unexpected mixed content")
          first ((,) []) $ runParseDTTotal (HProdCons pos <$> dtp) $ foldMap snd t

  nonTerminalE _ = id

  datatypeDT (Datatype prs _ lib name) = ParseDT $ \toks ->
    case Seq.viewl toks of
      Seq.EmptyL -> Left $ "missing " <> LT.pack (show name) <> " from library " <> LT.pack (show lib)
      t Seq.:< toks' -> (, toks') <$> prs t

  tokenDT spec = ParseDT $ \toks -> case Seq.viewl toks of
    Seq.EmptyL -> Left ("missing expected token: " <> spec)
    tok Seq.:< rest -> if spec == tok
      then Right ((), rest)
      else Left ("wrong token, expected: " <> tok)

instance RFunctor (EvenFlow (Parse pos)) where
  rfmap f = fmap (view f)

instance RPlus (EvenFlow (Parse pos)) where
  rempty = ParsePF $ \_ _ -> Left ([], "empty")
  rplus a as = ParsePF $ \rest attrs ->
    case runParsePF (HSumHere <$> a) rest attrs of
      Right res -> pure res
      --TODO: don't just throw away the LHS error
      Left _ -> runParsePF (HSumThere <$> as) rest attrs

instance RPlusApplyR (EvenFlow (Parse pos)) where
  type ActionR (EvenFlow (Parse pos)) = EvenFlow (Parse pos)
  rconsR pa pb = ParsePF $ \rest attrs -> do
    (a, rest', attrs') <- runParsePF pa rest attrs
    (as, rest'', attrs'') <- runParsePF pb rest' attrs'
    pure (HProdCons a as, rest'', attrs'')

instance RAlternative (EvenFlow (Parse pos)) where
  rnil = ParsePF $ \flow attrs -> pure (HProdNil, flow, attrs)

instance RFunctor (OddFlow (Parse pos)) where
  rfmap f = fmap (view f)

instance RPlus (OddFlow (Parse pos)) where
  rempty = ParseCF $ \_ _ -> Left ([], "empty")
  rplus a as = ParseCF $ \rest attrs ->
    case runParseCF (HSumHere <$> a) rest attrs of
      Right res -> pure res
      --TODO: don't just throw away the LHS error
      Left _ -> runParseCF (HSumThere <$> as) rest attrs

instance RPlusApplyR (OddFlow (Parse pos)) where
  type ActionR (OddFlow (Parse pos)) = EvenFlow (Parse pos)
  rconsR partial final = ParseCF $ \flow attrs -> case halfunsnocOddA flow of
    (pairs, t) -> do
      (a, pairs', attrs') <- runParsePF partial pairs attrs
      let flow' = halfsnocEvenA pairs' t
      (as, lft, attrs'') <- runParseCF final flow' attrs'
      pure (HProdCons a as, lft, attrs'')

instance RFunctor (El (Parse pos)) where
  rfmap f = fmap (view f)

--XXX: Do we need backtracking?
instance RPlus (El (Parse pos)) where
  rempty = ParseE $ const $ Left ([], "El zero")
  rplus a as = ParseE $ \el ->
    case runParseE (HSumHere <$> a) el of
      Right res -> pure res
      --TODO: don't just throw away the LHS error
      Left _ -> runParseE (HSumThere <$> as) el

instance RFunctor (DT (Parse pos)) where
  rfmap f = fmap (view f)

instance RPlus (DT (Parse pos)) where
  rempty = ParseDT $ const $ Left "empty"
  rplus a as = ParseDT $ \t ->
    case runParseDT (HSumHere <$> a) t of
      Right (res, toks) -> pure (res, toks)
      --TODO: don't just throw away the LHS error
      Left _ -> runParseDT (HSumThere <$> as) t

instance RPlusApplyR (DT (Parse pos)) where
  type ActionR (DT (Parse pos)) = DT (Parse pos)
  rconsR pa pb = ParseDT $ \toks -> do
    (a, toks') <- runParseDT pa toks
    (as, toks'') <- runParseDT pb toks'
    pure (HProdCons a as, toks'')

instance RAlternative (DT (Parse pos)) where
  rnil = ParseDT $ \toks -> Right (HProdNil, toks)

--TODO: stop throwing away errors
parse :: El (Parse pos) a -> Element pos -> Maybe a
parse p = hush . runParseE p
