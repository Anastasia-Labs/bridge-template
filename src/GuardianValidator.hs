{-# LANGUAGE TemplateHaskell #-}

module GuardianValidator (validator, PWitnessDatum (PWitnessDatum), PWitnessParametersD (..)) where

import Plutarch.Api.V2 (PAddress, PCurrencySymbol, PScriptHash, PScriptPurpose (PSpending), PTxInInfo, PValidator)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Prelude
import PlutusTx qualified

import Collection.Utils (paysToCredential, pheadSingleton, ppositiveSymbolValueOf, ptryOwnInput, (#>))
import Plutarch.Api.V1.Address (PCredential (PScriptCredential))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import PlutusLedgerApi.V2 (Address, BuiltinByteString, CurrencySymbol, ScriptHash)

data WitnessDatum = WitnessDatum
  { btcSent :: Integer
  , btcAddress :: BuiltinByteString
  , adaAddr :: Address
  }
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''WitnessDatum

data PWitnessDatum (s :: S)
  = PWitnessDatum
      ( Term
          s
          ( PDataRecord
              '[ "bridgeAmt" ':= PInteger
               , "otherChainAddr" ':= PByteString
               , "cardanoPKH" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PWitnessDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PWitnessDatum where
  type PLifted PWitnessDatum = WitnessDatum

deriving via
  (DerivePConstantViaData WitnessDatum PWitnessDatum)
  instance
    (PConstantDecl WitnessDatum)

instance PTryFrom PData PWitnessDatum

data GuardianRedeemer
  = ApproveWrap
  | DenyWrap
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''GuardianRedeemer

data PGuardianRedeemer (s :: S)
  = PApproveWrap (Term s (PDataRecord '[]))
  | PDenyWrap (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PGuardianRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PGuardianRedeemer where
  type PLifted PGuardianRedeemer = GuardianRedeemer

deriving via
  (DerivePConstantViaData GuardianRedeemer PGuardianRedeemer)
  instance
    (PConstantDecl GuardianRedeemer)

data WitnessParameters = WitnessParameters
  { multisigVH :: ScriptHash
  , multisigCert :: CurrencySymbol
  }
  deriving stock (Generic, Show)

data PWitnessParameters (s :: S) = PWitnessParameters
  { pmultisigVH :: Term s PScriptHash
  , pmultisigCert :: Term s PCurrencySymbol
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)

instance DerivePlutusType PWitnessParameters where
  type DPTStrat _ = PlutusTypeScott

-- w =  PWitnessParametersD $ pdcons @"multisigVH" # pdata (pconstant $ ScriptHash "adasda") #$ pdcons @"multisigCert" # pdata (pconstant $ CurrencySymbol "aaa") # pdnil
-- printTerm def (pcon w)
-- "(program 1.0.0 ((\\i0 -> constrData 0 (i1 (bData #616461736461) (i1 (bData #616161) [  ]))) (force mkCons)))"
-- disabling the below
-- Lucid error Emulator.tsx?04c9:34 Uncaught (in promise) Redeemer (Mint, 0): Failed to deserialise PlutusData using UnBData:
data PWitnessParametersD (s :: S)
  = PWitnessParametersD
      ( Term
          s
          ( PDataRecord
              '[ "multisigVH" ':= PAsData PScriptHash
               , "multisigCert" ':= PAsData PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PWitnessParametersD where
  type DPTStrat _ = PlutusTypeData

validator :: Term s ((PAsData PScriptHash) :--> (PAsData PCurrencySymbol) :--> PValidator)
validator = phoistAcyclic $
  plam $ \multisigVH multisigCert _ _ ctx -> unTermCont $ do
    contextFields <- pletFieldsC @["txInfo", "purpose"] ctx
    PSpending ownRef' <- pmatchC contextFields.purpose
    ownRef <- pletC $ pfield @"_0" # ownRef'

    txInfoFields <- pletFieldsC @'["inputs", "outputs", "mint", "signatories", "datums"] contextFields.txInfo

    ownInput <- pletC $ ptryOwnInput # txInfoFields.inputs # ownRef
    ownInputFields <- pletFieldsC @["address", "value"] ownInput
    PScriptCredential ownInputScriptCredential <- pmatchC (pfield @"credential" # ownInputFields.address)
    ownValHash <- pletC (pfield @"_0" # ownInputScriptCredential)

    pmultisigValHash <- pletC $ pfromData multisigVH
    let noScriptOutputs = pnull #$ pfilter # (paysToCredential # ownValHash) # txInfoFields.outputs
        isSigInp = plam (\txinp -> pletFields @'["resolved"] txinp $ \txInFields -> paysToCredential # pmultisigValHash # txInFields.resolved)
        sigInput' :: Term _ PTxInInfo = pheadSingleton #$ pfilter # isSigInp # txInfoFields.inputs
        sigInput = pfield @"resolved" # sigInput'

    sigInputF <- pletFieldsC @'["value"] sigInput
    let checkSigInp = ppositiveSymbolValueOf # pfromData multisigCert # sigInputF.value #> 0

    pure $
      popaque $
        pif
          ( (ptraceIfFalse "GuardianValidator f1" noScriptOutputs)
              #&& (ptraceIfFalse "GuardianValidator f2" checkSigInp)
          )
          (pconstant ())
          perror
