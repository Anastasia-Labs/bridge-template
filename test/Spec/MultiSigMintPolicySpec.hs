-- MultiSigMintPolicySpec tests the MultiSig Mint Policy for a smart contract.
-- The module sets up various testing scenarios
-- to ensure the minting policy behaves as expected under different conditions.

module Spec.MultiSigMintPolicySpec (
  unitTest,
) where

import MultiSigMintPolicy (policy)
import MultiSigValidator (MultisigDatum (..))
import Plutarch.Context (
  UTXO,
  address,
  buildMinting',
  input,
  mint,
  output,
  withInlineDatum,
  withMinting,
  withRefIndex,
  withRefTxId,
  withValue,
 )
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  CurrencySymbol,
  PubKeyHash (..),
  ScriptContext,
  ScriptHash,
  TokenName (..),
  TxOutRef (..),
  Value,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)

pubkeyhash :: PubKeyHash
pubkeyhash = "94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"

txOutRef :: TxOutRef
txOutRef = TxOutRef "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d" 1

scriptHash :: ScriptHash
scriptHash = "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e"

multisigValAddress :: Address
multisigValAddress =
  Address
    (ScriptCredential "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e")
    Nothing

currencySymbol :: CurrencySymbol
currencySymbol = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

tokenName :: TokenName
tokenName = "token"

mintedValue :: Value
mintedValue = singleton currencySymbol tokenName 1

inputUTXO :: UTXO
inputUTXO =
  mconcat
    [ withValue (singleton "" "" 4_000_000)
    , withRefTxId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
    , withRefIndex 1
    ]

outputUTXO :: UTXO
outputUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 9_000_000 <> mintedValue)
    , withInlineDatum $
        MultisigDatum
          { keys = ["94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"]
          , requiredCount = 1
          }
    ]

goodCtx :: ScriptContext
goodCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , mint mintedValue
      , withMinting currencySymbol
      ]

invalidOutputUTXO :: UTXO
invalidOutputUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 9_000_000 <> mintedValue)
    ]

invalidOutputCtx :: ScriptContext
invalidOutputCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output invalidOutputUTXO
      , mint mintedValue
      , withMinting currencySymbol
      ]

wrongMintedValueCtx :: ScriptContext
wrongMintedValueCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , mint wrongMintedValue
      , withMinting currencySymbol
      ]

wrongMintedValue :: Value
wrongMintedValue = singleton currencySymbol tokenName 2

wrongBurntValue :: Value
wrongBurntValue = singleton currencySymbol tokenName (-1)

wrongBurntValueCtx :: ScriptContext
wrongBurntValueCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , mint mintedValue
      , mint wrongBurntValue
      , withMinting currencySymbol
      ]

wrongOutputDatumUTXO :: UTXO
wrongOutputDatumUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 9_000_000 <> mintedValue)
    , withInlineDatum $
        MultisigDatum
          { keys = ["ae40117d0a868fe89e7af0f95e52a43759494907d6b58e7587ff7212"]
          , requiredCount = 1
          }
    ]

wrongOutputDatumCtx :: ScriptContext
wrongOutputDatumCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output wrongOutputDatumUTXO
      , mint mintedValue
      , withMinting currencySymbol
      ]

wrongCountOutputDatumUTXO :: UTXO
wrongCountOutputDatumUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 9_000_000 <> mintedValue)
    , withInlineDatum $
        MultisigDatum
          { keys = ["94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"]
          , requiredCount = 2
          }
    ]

wrongCountOutputDatumCtx :: ScriptContext
wrongCountOutputDatumCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output wrongCountOutputDatumUTXO
      , mint mintedValue
      , withMinting currencySymbol
      ]

unitTest :: TestTree
unitTest = tryFromPTerm "MultiSig Mint Policy Unit Test" (policy # pdata (pconstant pubkeyhash) # pdata (pconstant scriptHash) # pconstant txOutRef) $ do
  testEvalCase
    "Pass - Policy"
    Success
    [ PlutusTx.toData ()
    , PlutusTx.toData goodCtx
    ]
  testEvalCase
    "Failure - Invalid output UTXO"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData invalidOutputCtx
    ]
  testEvalCase
    "Failure - Wrong minted value"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData wrongMintedValueCtx
    ]
  testEvalCase
    "Failure - Wrong burnt value"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData wrongBurntValueCtx
    ]
  testEvalCase
    "Failure - Wrong output datum"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData wrongOutputDatumCtx
    ]
  testEvalCase
    "Failure - Wrong count output datum"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData wrongCountOutputDatumCtx
    ]
