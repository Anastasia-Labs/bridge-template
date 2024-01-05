module Spec.MultiSigValidatorSpec (
  unitTest,
)
where

import MultiSigValidator (
  MultisigDatum (..),
  MultisigRedeemer (..),
  validator,
 )
import Plutarch.Context (
  UTXO,
  address,
  buildSpending',
  input,
  output,
  signedWith,
  withInlineDatum,
  withRefIndex,
  withRefTxId,
  withSpendingOutRefId,
  withValue,
 )
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  ScriptContext,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)

multisigValAddress :: Address
multisigValAddress =
  Address
    (ScriptCredential "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e")
    Nothing

inputDatum :: MultisigDatum
inputDatum =
  MultisigDatum
    { keys = ["94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"]
    , requiredCount = 1
    }

inputUTXO :: UTXO
inputUTXO =
  mconcat
    [ address multisigValAddress
    , withRefTxId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
    , withRefIndex 1
    , withValue (singleton "" "" 9_000_000)
    , withInlineDatum inputDatum
    ]

outputUTXO :: UTXO
outputUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 9_000_000)
    , withInlineDatum $
        MultisigDatum
          { keys = ["94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"]
          , requiredCount = 1
          }
    ]

goodCtx :: ScriptContext
goodCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , withSpendingOutRefId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , signedWith "94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"
      ]

missSignatureCtx :: ScriptContext
missSignatureCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , withSpendingOutRefId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      ]

invalidRequiredCountOutputUTXO :: UTXO
invalidRequiredCountOutputUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 9_000_000)
    , withInlineDatum $
        MultisigDatum
          { keys = ["94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"]
          , requiredCount = -1
          }
    ]

invalidRequiredCountOutputCtx :: ScriptContext
invalidRequiredCountOutputCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , output invalidRequiredCountOutputUTXO
      , withSpendingOutRefId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , signedWith "94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"
      ]

invalidNewKeyCountUTXO :: UTXO
invalidNewKeyCountUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 9_000_000)
    , withInlineDatum $
        MultisigDatum
          { keys = ["94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594", "40117d0a868fe89e7af0f95e52a43759494907d6b58e7587ff7212ae"]
          , requiredCount = 3
          }
    ]

invalidNewKeyCountCtx :: ScriptContext
invalidNewKeyCountCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , output invalidNewKeyCountUTXO
      , withSpendingOutRefId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , signedWith "94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"
      ]

wrongOutputDatumUTXO :: UTXO
wrongOutputDatumUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 9_000_000)
    , withInlineDatum $
        MultisigDatum
          { keys = ["94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"]
          , requiredCount = 2
          }
    ]

wrongOutputDatumCtx :: ScriptContext
wrongOutputDatumCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , output wrongOutputDatumUTXO
      , withSpendingOutRefId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , signedWith "94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"
      ]

unitTest :: TestTree
unitTest = tryFromPTerm "MultiSig Validator Unit Test" validator $ do
  testEvalCase
    "Pass - Update Validation"
    Success
    [ PlutusTx.toData inputDatum
    , PlutusTx.toData Update
    , PlutusTx.toData goodCtx
    ]
  testEvalCase
    "Failure - Update Validation - miss signatories"
    Failure
    [ PlutusTx.toData inputDatum
    , PlutusTx.toData Update
    , PlutusTx.toData missSignatureCtx
    ]
  testEvalCase
    "Failure - Update Validation - invalid new required count"
    Failure
    [ PlutusTx.toData inputDatum
    , PlutusTx.toData Update
    , PlutusTx.toData invalidRequiredCountOutputCtx
    ]
  testEvalCase
    "Failure - Update Validation - invalid new key count"
    Failure
    [ PlutusTx.toData inputDatum
    , PlutusTx.toData Update
    , PlutusTx.toData invalidNewKeyCountCtx
    ]
  testEvalCase
    "Pass - Sign Validation"
    Success
    [ PlutusTx.toData inputDatum
    , PlutusTx.toData Sign
    , PlutusTx.toData goodCtx
    ]
  testEvalCase
    "Failure - Sign Validation - miss signatories"
    Failure
    [ PlutusTx.toData inputDatum
    , PlutusTx.toData Sign
    , PlutusTx.toData missSignatureCtx
    ]
  testEvalCase
    "Failure - Sign Validation - wrong output datum"
    Failure
    [ PlutusTx.toData inputDatum
    , PlutusTx.toData Sign
    , PlutusTx.toData wrongOutputDatumCtx
    ]
