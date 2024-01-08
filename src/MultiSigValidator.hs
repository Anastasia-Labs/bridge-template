{-# LANGUAGE TemplateHaskell #-}

module MultiSigValidator (
  validator,
  PMultisigDatum (PMultisigDatum),
  MultisigDatum (..),
  MultisigRedeemer (..),
) where

import Collection.Utils (paysToCredential, pheadSingleton, ptryOwnInput, pvalueContains, (#>), (#>=))
import Plutarch.Api.V1.Address (PCredential (PScriptCredential))
import Plutarch.Api.V2 (
  POutputDatum (POutputDatum),
  PPubKeyHash,
  PScriptPurpose (PSpending),
  PValidator,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusLedgerApi.V2 (PubKeyHash)
import PlutusTx qualified
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptryFromC)

data MultisigDatum = MultisigDatum
  { keys :: [PubKeyHash]
  , requiredCount :: Integer
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''MultisigDatum [('MultisigDatum, 0)]
PlutusTx.makeLift ''MultisigDatum

newtype PMultisigDatum (s :: S)
  = PMultisigDatum
      ( Term
          s
          ( PDataRecord
              '[ "keys" ':= PBuiltinList (PAsData PPubKeyHash)
               , "requiredCount" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)
instance PUnsafeLiftDecl PMultisigDatum where type PLifted PMultisigDatum = MultisigDatum
deriving via (DerivePConstantViaData MultisigDatum PMultisigDatum) instance PConstantDecl MultisigDatum
instance PTryFrom PData PMultisigDatum
instance DerivePlutusType PMultisigDatum where
  type DPTStrat _ = PlutusTypeData

data MultisigRedeemer
  = Update
  | Sign
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''MultisigRedeemer
  [ ('Update, 0)
  , ('Sign, 1)
  ]
PlutusTx.makeLift ''MultisigRedeemer

data PMultisigRedeemer (s :: S)
  = PUpdate (Term s (PDataRecord '[]))
  | PSign (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

deriving via
  (DerivePConstantViaData MultisigRedeemer PMultisigRedeemer)
  instance
    PConstantDecl MultisigRedeemer

instance PUnsafeLiftDecl PMultisigRedeemer where
  type PLifted PMultisigRedeemer = MultisigRedeemer

instance PTryFrom PData PMultisigRedeemer

instance DerivePlutusType PMultisigRedeemer where
  type DPTStrat _ = PlutusTypeData

pnoDuplicates ::
  forall (list :: PType -> PType) (a :: PType).
  (PListLike list, PEq a, PElemConstraint list a) =>
  ClosedTerm (list a :--> PBool)
pnoDuplicates =
  phoistAcyclic $
    plam $ \l ->
      let pnoDuplicates' :: (PListLike list, PEq a, PElemConstraint list a) => ClosedTerm (list a :--> list a :--> PBool)
          pnoDuplicates' =
            pfix
              #$ plam
                ( \self l xs ->
                    pelimList (\y ys -> pif (pelem # y # xs) (pconstant False) (self # ys # (pcons # y # xs))) (pconstant True) l
                )
       in pnoDuplicates' # l # (pnil @list)

psignedByAMajority :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PInteger :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
psignedByAMajority = phoistAcyclic $ plam $ \allKeys requiredCount signers ->
  plength # (pfilter # plam (\sig -> pelem # sig # allKeys) # signers) #>= requiredCount

validator :: ClosedTerm PValidator
validator = phoistAcyclic $ plam $ \dat' redeemer' ctx -> unTermCont $ do
  contextFields <- pletFieldsC @["txInfo", "purpose"] ctx
  PSpending ownRef' <- pmatchC contextFields.purpose
  ownRef <- pletC $ pfield @"_0" # ownRef'

  txInfoFields <- pletFieldsC @["inputs", "outputs", "signatories"] contextFields.txInfo

  ownInput <- pletC $ ptryOwnInput # txInfoFields.inputs # ownRef
  ownInputFields <- pletFieldsC @["address", "value"] ownInput
  PScriptCredential ownInputScriptCredential <- pmatchC (pfield @"credential" # ownInputFields.address)
  ownValHash <- pletC (pfield @"_0" # ownInputScriptCredential)

  let ownOutput = pheadSingleton #$ pfilter # (paysToCredential # ownValHash) # txInfoFields.outputs
  ownOutputFields <- pletFieldsC @["value", "datum"] ownOutput
  POutputDatum ownOutputD <- pmatchC ownOutputFields.datum
  ownOutputDatum <- pletC $ pfromPDatum @PMultisigDatum # (pfield @"outputDatum" # ownOutputD)
  dat <- fst <$> ptryFromC @PMultisigDatum dat'
  datF <- pletFieldsC @["keys", "requiredCount"] dat
  redeemer <- fst <$> ptryFromC @PMultisigRedeemer redeemer'
  pure $
    popaque $
      pif
        ( ptraceIfFalse "MultiSigValidator f1" (psignedByAMajority # datF.keys # datF.requiredCount # txInfoFields.signatories)
            #&& ptraceIfFalse "MultiSigValidator f2" (pvalueContains # ownOutputFields.value # ownInputFields.value)
            #&& pmatch
              redeemer
              ( \case
                  PUpdate _ ->
                    pletFields @'["keys", "requiredCount"] ownOutputDatum $ \newDatumF ->
                      plet (plength # pfromData newDatumF.keys) $ \newKeyCount ->
                        ptraceIfFalse "MultiSigValidator f3" (newKeyCount #> 0)
                          #&& ptraceIfFalse "MultiSigValidator f4" (pfromData newDatumF.requiredCount #> 0)
                          #&& ptraceIfFalse "MultiSigValidator f5" (pfromData newDatumF.requiredCount #<= newKeyCount)
                          #&& ptraceIfFalse "MultiSigValidator f6" (pnoDuplicates # pfromData newDatumF.keys)
                  PSign _ -> ptraceIfFalse "MultiSigValidator f7" (dat #== ownOutputDatum)
              )
        )
        (pconstant ())
        perror

-- pure $
--   popaque $
--     pif
--       ( ptraceIfFalse "MultiSigValidator f1" (psignedByAMajority # datF.keys # datF.requiredCount # txInfoFields.signatories)
--           -- TODO: Only non Ada Values should be compared
--           #&& ptraceIfFalse "MultiSigValidator f2" (ownInputFields.value #== ownOutputFields.value)
--           #&& ( pmatch
--                   redeemer
--                   ( \case
--                       PUpdate _ ->
--                         pletFields @'["keys", "requiredCount"] ownOutputDatum $ \newDatumF ->
--                           plet (plength # pfromData newDatumF.keys) $ \newKeyCount ->
--                             ptraceIfFalse "MultiSigValidator f3" (newKeyCount #> 0)
--                               #&& ptraceIfFalse "MultiSigValidator f4" (pfromData newDatumF.requiredCount #> 0)
--                               #&& ptraceIfFalse "MultiSigValidator f5" (pfromData newDatumF.requiredCount #<= newKeyCount)
--                               #&& ptraceIfFalse "MultiSigValidator f6" (pnoDuplicates # pfromData newDatumF.keys)
--                       PSign _ -> ptraceIfFalse "MultiSigValidator f7" (dat #== ownOutputDatum)
--                   )
--               )
--       )
--       (pconstant ())
--       perror
