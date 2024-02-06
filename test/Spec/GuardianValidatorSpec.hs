-- This module defines unit tests for the Guardian Validator smart contract.
-- It includes setup for test data, such as UTXOs and script contexts,
-- and specifies test cases to validate the contract's behavior
-- under various conditions.

module Spec.GuardianValidatorSpec (
  unitTest,
)
where

import GuardianValidator (validator)
import Plutarch.Context (
  UTXO,
  address,
  buildSpending',
  input,
  output,
  withRefIndex,
  withRefTxId,
  withSpendingOutRefId,
  withValue,
 )
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  CurrencySymbol,
  ScriptContext,
  ScriptHash,
  StakingCredential (..),
  Value,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)

currencySymbol :: CurrencySymbol
currencySymbol = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

inputValue :: Value
inputValue = singleton currencySymbol "" 10

scriptHash :: ScriptHash
scriptHash = "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e"

multiSigValAddr :: Address
multiSigValAddr =
  let cred = "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e"
      stakeCred = PubKeyCredential "b1f2f20a8781a3ba967d8c7b5068d21d799e809dcce22f651679d661"
   in Address (ScriptCredential cred) (Just (StakingHash stakeCred))

multiSigValUTXO :: UTXO
multiSigValUTXO =
  mconcat
    [ address multiSigValAddr
    , withValue (singleton "" "" 4_000_000 <> inputValue)
    , withRefTxId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
    , withRefIndex 1
    ]

goodCtx :: ScriptContext
goodCtx =
  buildSpending' $
    mconcat
      [ input multiSigValUTXO
      , withSpendingOutRefId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      ]

fakeMultiSigValAddr :: Address
fakeMultiSigValAddr =
  let cred = "046b8b85632ba47c664c8f924b777e22e380f1157b688ac08f26a64e"
      stakeCred = PubKeyCredential "b1f2f20a8781a3ba967d8c7b5068d21d799e809dcce22f651679d661"
   in Address (ScriptCredential cred) (Just (StakingHash stakeCred))

fakeMultiSigValUTXO :: UTXO
fakeMultiSigValUTXO =
  mconcat
    [ address fakeMultiSigValAddr
    , withValue (singleton "" "" 4_000_000 <> inputValue)
    , withRefTxId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
    , withRefIndex 1
    ]

fakeMultisigUTXOCtx :: ScriptContext
fakeMultisigUTXOCtx =
  buildSpending' $
    mconcat
      [ input fakeMultiSigValUTXO
      , withSpendingOutRefId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      ]

outputMultiSigValUTXO :: UTXO
outputMultiSigValUTXO =
  mconcat
    [ address multiSigValAddr
    , withValue (singleton "" "" 10_000_000 <> inputValue)
    , withRefTxId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
    , withRefIndex 1
    ]

invalidOutputCtx :: ScriptContext
invalidOutputCtx =
  buildSpending' $
    mconcat
      [ input multiSigValUTXO
      , output outputMultiSigValUTXO
      , withSpendingOutRefId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      ]

unitTest :: TestTree
unitTest = tryFromPTerm "Guardian Validator Unit Test" (validator # pdata (pconstant scriptHash) # pdata (pconstant currencySymbol)) $ do
  testEvalCase
    "Pass - Validation"
    Success
    [ PlutusTx.toData ()
    , PlutusTx.toData ()
    , PlutusTx.toData goodCtx
    ]
  testEvalCase
    "Failure - Invalid multisig input UTXO"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData ()
    , PlutusTx.toData fakeMultisigUTXOCtx
    ]
  testEvalCase
    "Failure - Invalid multisig output UTXO"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData ()
    , PlutusTx.toData invalidOutputCtx
    ]
