{-# LANGUAGE TemplateHaskell #-}

module MultiSigMintPolicy (policy) where

import MultiSigValidator (PMultisigDatum)
import Plutarch.Api.V2 (PMintingPolicy, POutputDatum (POutputDatum), PPubKeyHash, PScriptHash, PScriptPurpose (PMinting), PTxOutRef)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusTx qualified

import Collection.Utils (paysToCredential, phasInput, pheadSingleton, pnegativeSymbolValueOf, ppositiveSymbolValueOf)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptryFromC)

data GuardianMintAction
  = MintGuardianCrt
  | BurnGuardianCrt
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''GuardianMintAction

data PGuardianMintAction (s :: S)
  = PMintGuardianCrt (Term s (PDataRecord '[]))
  | PBurnGuardianCrt (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PGuardianMintAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PGuardianMintAction where
  type PLifted PGuardianMintAction = GuardianMintAction

deriving via (DerivePConstantViaData GuardianMintAction PGuardianMintAction) instance (PConstantDecl GuardianMintAction)

instance PTryFrom PData PGuardianMintAction
instance PTryFrom PData (PAsData PGuardianMintAction)

-- pGuardianSignerPKH :: Term s PPubKeyHash
-- pGuardianSignerPKH = pconstant (PubKeyHash "01c15871cfb766e1862abfb6eb55e1e9d890e7e8e76ab4f3fef7ae281f")

policy :: Term s ((PAsData PPubKeyHash) :--> (PAsData PScriptHash) :--> PTxOutRef :--> PMintingPolicy)
policy = phoistAcyclic $
  plam $ \guardianSignerPKH multisigVH oref redm' context -> unTermCont $ do
    contextFields <- pletFieldsC @["txInfo", "purpose"] context
    PMinting ((pfield @"_0" #) -> ownPolicyId) <- pmatchC contextFields.purpose
    txInfoFields <- pletFieldsC @["inputs", "outputs", "mint"] contextFields.txInfo
    mintedRTs <- pletC $ ppositiveSymbolValueOf # ownPolicyId # txInfoFields.mint
    burnedRTs <- pletC $ pnegativeSymbolValueOf # ownPolicyId # txInfoFields.mint

    let msOutput = pheadSingleton # (pfilter # (paysToCredential # (pfromData multisigVH)) # txInfoFields.outputs)
    msOutputFields <- pletFieldsC @["value", "datum"] msOutput

    POutputDatum msOutputDatum' <- pmatchC msOutputFields.datum
    let msOutputDatum = pfromPDatum @PMultisigDatum # (pfield @"outputDatum" # msOutputDatum')
    msDatumF <- pletFieldsC @["keys", "requiredCount"] msOutputDatum
    let correctDatum = (pheadSingleton # msDatumF.keys #== guardianSignerPKH) #&& pfromData msDatumF.requiredCount #== 1
        isUtxoSpent = phasInput # txInfoFields.inputs # oref
    redm <- fst <$> ptryFromC @PGuardianMintAction redm'
    pure $
      popaque $
        pif
          ( pmatch
              redm
              ( \case
                  PMintGuardianCrt _ ->
                    ptraceIfFalse "MultiSigMintPolicy f1" (mintedRTs #== 1)
                      #&& ptraceIfFalse "MultiSigMintPolicy f2" (burnedRTs #== 0)
                      #&& ptraceIfFalse "MultiSigMintPolicy f3" (correctDatum)
                      #&& ptraceIfFalse "MultiSigMintPolicy f4" (isUtxoSpent)
                  PBurnGuardianCrt _ ->
                    ptraceIfFalse "MultiSigMintPolicy f5" (mintedRTs #== 0)
                      #&& ptraceIfFalse "MultiSigMintPolicy f6" (burnedRTs #== 1)
              )
          )
          (pconstant ())
          perror
