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
  ScriptHash(..),
  StakingCredential (..),
  Value,
  -- TxId(..),
  singleton, 
  -- toData
 )
import PlutusTx qualified
import Test.Tasty (TestTree)
-- import Test.Tasty.QuickCheck (Gen, Property, forAll, listOf1, elements, choose, vectorOf, (===), counterexample)
-- import Data.ByteString.Char8 (pack)
-- import PlutusTx.Builtins (BuiltinByteString, toBuiltin)

currencySymbol :: CurrencySymbol
currencySymbol = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

inputValue :: Value
inputValue = singleton currencySymbol "" 10

scriptHash :: ScriptHash
scriptHash = "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e"

multiSigValAddr :: Address
multiSigValAddr =
  let stakeCred = PubKeyCredential "b1f2f20a8781a3ba967d8c7b5068d21d799e809dcce22f651679d661"
   in Address (ScriptCredential scriptHash) (Just (StakingHash stakeCred))

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



-- -- Generate a random ByteString of a given length
-- genByteString :: Int -> Gen BuiltinByteString
-- genByteString len = toBuiltin . pack <$> vectorOf len (elements (['a'..'z'] ++ ['0'..'9']))

-- -- Generate a random Value within a specified range
-- genValue :: Integer -> Integer -> Gen Value
-- genValue minValue maxValue = do
--     amount <- choose (minValue, maxValue)
--     return $ singleton currencySymbol "" amount

-- -- Generate a random Address (simplified version)
-- genAddress :: Gen Address
-- genAddress = do
--     bs <- genByteString 32
--     return $ Address (ScriptCredential $ ScriptHash bs ) Nothing

-- -- Generate a random UTXO
-- genUTXO :: Gen UTXO
-- genUTXO = do
--     addr <- genAddress
--     val <- genValue 1000000 10000000  -- Adjust the range according to your needs
--     refTxId <- genByteString 32  -- Simulating a transaction ID
--     refIdx <- choose (0, 10)  -- Simulating an output index in a transaction
--     return $ mconcat
--         [ address addr
--         , withValue val
--         , withRefTxId (TxId refTxId)
--         , withRefIndex refIdx
--         ]

-- -- Generate a valid ScriptContext
-- genValidScriptContext :: Gen ScriptContext
-- genValidScriptContext =  buildSpending' . input <$> genUTXO

-- -- Generate an invalid ScriptContext
-- genInvalidScriptContext :: Gen ScriptContext
-- genInvalidScriptContext = buildSpending' . input <$> genUTXO

-- prop_validatorSuccess :: Property
-- prop_validatorSuccess = forAll genValidScriptContext $ \ctx ->
--   let result = (validator # pdata (pconstant scriptHash) # pdata (pconstant currencySymbol))  (PlutusTx.toData ()) (PlutusTx.toData ()) (PlutusTx.toData ctx)
--   in counterexample ("Validator failed for valid context: " ++ show ctx) $ result === True

-- prop_validatorFailure :: Property
-- prop_validatorFailure = forAll genInvalidScriptContext $ \ctx ->
--   let result = (validator # pdata (pconstant scriptHash) # pdata (pconstant currencySymbol)) (PlutusTx.toData ()) (PlutusTx.toData ()) (PlutusTx.toData ctx)
--   in counterexample ("Validator succeeded for invalid context: " ++ show ctx) $ result === False
