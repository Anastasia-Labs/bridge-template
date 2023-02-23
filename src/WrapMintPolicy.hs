{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module WrapMintPolicy (policy) where

import GuardianValidator (PWitnessDatum)
import Plutarch.Api.V1.Address (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, POutputDatum (POutputDatum), PPubKeyHash, PScriptHash, PScriptPurpose (PMinting), PTokenName, PTxOut)
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC, ptryFromC)
import Plutarch.Prelude

import Collection.Utils (paysToCredential, pheadSingleton, pnegativeSymbolValueOf, ppositiveSymbolValueOf, (#>))
import Plutarch.Api.V1.Value (pvalueOf)

data PMintBTCParameters (s :: S) = PMintBTCParameters
  { pguardianVH :: Term s PScriptHash
  -- , pmultisigCert :: Term s PCurrencySymbol
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)

instance DerivePlutusType PMintBTCParameters where
  type DPTStrat _ = PlutusTypeScott

data PMintBTCAction (s :: S)
  = PMintBTC (Term s (PDataRecord '[]))
  | PBurnBTC (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PMintBTCAction where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMintBTCAction

paysAmountToPkh :: Term s (PTokenName :--> PInteger :--> PCurrencySymbol :--> PPubKeyHash :--> PTxOut :--> PBool)
paysAmountToPkh = phoistAcyclic $
  plam $ \tn amt ownCS userPKH txOut -> unTermCont $ do
    txOutFields <- tcont $ pletFields @["address", "value"] txOut
    let cred = pfield @"credential" # txOutFields.address
        result = pmatch cred $ \case
          PPubKeyCredential ((pfield @"_0" #) -> pkh) -> (pkh #== userPKH) #&& (amt #<= (pvalueOf # txOutFields.value # ownCS # tn))
          PScriptCredential _ -> pcon PFalse
    pure result

policy :: Term s ((PAsData PTokenName) :--> (PAsData PScriptHash) :--> PMintingPolicy)
policy = phoistAcyclic $ plam $ \bridgeTn guardianValHash redeemer' ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["purpose", "txInfo"] ctx
  infoF <- pletFieldsC @'["inputs", "outputs", "mint"] ctxF.txInfo
  PMinting policy <- pmatchC $ pfromData ctxF.purpose
  let ownPolicyId = pfield @"_0" # policy

  mintedCS <- pletC $ ppositiveSymbolValueOf # ownPolicyId # infoF.mint
  burnedCS <- pletC $ pnegativeSymbolValueOf # ownPolicyId # infoF.mint
  redeemer <- fst <$> ptryFromC @PMintBTCAction redeemer'
  pure $
    popaque $
      pif
        ( pmatch redeemer $ \case
            PMintBTC _ -> unTermCont $ do
              let isGuardianInp =
                    plam
                      ( \txinp ->
                          pletFields @'["resolved"] txinp $ \txInFields ->
                            paysToCredential # (pfromData guardianValHash) # txInFields.resolved
                      )
              guardianInput <- pletC $ pheadSingleton #$ pfilter # isGuardianInp # pfromData infoF.inputs
              guardianInputF <- pletFieldsC @["address", "value", "datum"] (pfield @"resolved" # guardianInput)
              POutputDatum guardianInpDatum' <- pmatchC guardianInputF.datum
              let guardianInpDatum = pfromPDatum @PWitnessDatum # (pfield @"outputDatum" # guardianInpDatum')
              guardianDatumF <- pletFieldsC @["cardanoPKH", "bridgeAmt"] guardianInpDatum
              gbridgeAmt <- pletC $ pfromData guardianDatumF.bridgeAmt
              PPubKeyCredential ((pfield @"_0" #) -> cardanoPKH) <- pmatchC $ pfield @"credential" # pfromData guardianDatumF.cardanoPKH
              ptraceC (pshow cardanoPKH) -- TODO: remove this
              pure $
                ptraceIfFalse "WrapMintPolicy f1" (mintedCS #== gbridgeAmt)
                  #&& ptraceIfFalse "WrapMintPolicy f2" (pany # (paysAmountToPkh # (pfromData bridgeTn) # gbridgeAmt # ownPolicyId # cardanoPKH) # pfromData infoF.outputs)
                  #&& ptraceIfFalse "WrapMintPolicy f3" (burnedCS #== 0)
            PBurnBTC _ ->
              ptraceIfFalse "WrapMintPolicy f4" (mintedCS #== 0)
                #&& ptraceIfFalse "WrapMintPolicy f5" (burnedCS #< 0)
        )
        (pconstant ())
        perror
