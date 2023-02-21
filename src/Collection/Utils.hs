{-# OPTIONS_GHC -Wno-unused-imports #-}

module Collection.Utils (
  tcexpectJust,
  paysToCredential,
  ptryOwnInput,
  pheadSingleton,
  psymbolValueOfHelper,
  ppositiveSymbolValueOf,
  pnegativeSymbolValueOf,
  pcountOfUniqueTokens,
  pfirstTokenName,
  phasInput,
  (#>),
  (#>=),
  pvalueContains,
)
where

import Plutarch.Monadic qualified as P

import "liqwid-plutarch-extra" Plutarch.Extra.List (plookupAssoc)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pmatchC)

import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (AmountGuarantees (Positive), KeyGuarantees (Sorted), PCurrencySymbol, PMap (PMap), PScriptHash, PTokenName, PTxInInfo, PTxOut, PTxOutRef, PValue (PValue))
import Plutarch.Prelude

tcexpectJust :: forall r (a :: PType) (s :: S). Term s r -> Term s (PMaybe a) -> TermCont @r s (Term s a)
tcexpectJust escape ma = tcont $ \f -> pmatch ma $ \case
  PJust v -> f v
  PNothing -> escape

paysToCredential :: Term s (PScriptHash :--> PTxOut :--> PBool)
paysToCredential = phoistAcyclic $
  plam $ \valHash txOut -> unTermCont $ do
    let txOutCred = pfield @"credential" # (pfield @"address" # txOut)
    pure $
      pmatch txOutCred $ \case
        PScriptCredential txOutValHash -> (pfield @"_0" # txOutValHash) #== valHash
        PPubKeyCredential _ -> (pcon PFalse)

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs

-- Get the head of the list if the list contains exactly one element, otherwise error.
pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList (\x xs -> pif (pnull # xs) x (ptraceError "List contains more than one element.")) perror xs

psymbolValueOfHelper ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( (PInteger :--> PBool)
        :--> PCurrencySymbol
        :--> ( PValue keys amounts
                :--> PInteger
             )
    )
psymbolValueOfHelper =
  phoistAcyclic $
    plam $ \cond sym value'' -> unTermCont $ do
      PValue value' <- pmatchC value''
      PMap value <- pmatchC value'
      m' <-
        tcexpectJust
          0
          ( plookupAssoc
              # pfstBuiltin
              # psndBuiltin
              # pdata sym
              # value
          )
      PMap m <- pmatchC (pfromData m')
      pure $
        pfoldr
          # plam
            ( \x v ->
                plet (pfromData $ psndBuiltin # x) $ \q ->
                  pif
                    (cond # q)
                    (q + v)
                    v
            )
          # 0
          # m

-- | @since 1.0.0
ppositiveSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
ppositiveSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (0 #<)

-- | @since 1.0.0
pnegativeSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
pnegativeSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (#< 0)

-- | Probably more effective than `plength . pflattenValue`
pcountOfUniqueTokens ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountOfUniqueTokens = phoistAcyclic $
  plam $ \val ->
    let tokensLength = plam (\pair -> pmatch (pfromData $ psndBuiltin # pair) $ \(PMap tokens) -> plength # tokens)
     in pmatch val $ \(PValue val') ->
          pmatch val' $ \(PMap csPairs) -> pfoldl # plam (\acc x -> acc + (tokensLength # x)) # 0 # csPairs

pfirstTokenName ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PTokenName)
pfirstTokenName = phoistAcyclic $
  plam $ \val ->
    pmatch val $ \(PValue val') ->
      pmatch val' $ \(PMap csPairs) ->
        pmatch (pfromData (psndBuiltin # (phead # csPairs))) $ \(PMap tokens) ->
          pfromData $ pfstBuiltin # (phead # tokens)

phasInput :: Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PBool)
phasInput = phoistAcyclic $ plam $ \refs oref -> pany # plam (\oref' -> oref #== pfield @"outRef" # oref') # refs

(#>) :: POrd t => Term s t -> Term s t -> Term s PBool
a #> b = b #< a

infix 4 #>

(#>=) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a

infix 4 #>=

pvalueContains ::
  ClosedTerm
    ( PValue 'Sorted 'Positive
        :--> PValue 'Sorted 'Positive
        :--> PBool
    )
pvalueContains = phoistAcyclic $
  plam $ \superset subset -> P.do
    let forEachCS = plam $ \csPair ->
          let cs = pfromData $ pfstBuiltin # csPair
              tnMap = pto $ pfromData $ psndBuiltin # csPair
           in pall # forEachTN cs # tnMap

        forEachTN cs = plam $ \tnPair ->
          let tn = pfromData $ pfstBuiltin # tnPair
              amount = pfromData $ psndBuiltin # tnPair
           in amount #<= pvalueOf # superset # cs # tn

    pall # forEachCS #$ pto $ pto subset
