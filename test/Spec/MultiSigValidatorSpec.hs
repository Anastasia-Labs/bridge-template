-- MultiSigValidatorSpec module, designed for testing the MultiSig Validator smart contract functionality.
-- The module includes setup for test data and specifies test cases
-- to validate the contract's behavior under various conditions.

module Spec.MultiSigValidatorSpec (
  unitTest,
)
where

import MultiSigValidator (
  MultisigDatum (..),
  MultisigRedeemer (..),
  validator,
  pnoDuplicates
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
import Test.Tasty (TestTree, testGroup)

import Test.Tasty.QuickCheck (Gen, Property, listOf, listOf1, elements, shuffle, testProperty, (===), forAll, arbitrary)
import qualified PlutusTx.AssocMap as M
import Plutarch.Prelude
import Data.List(nub)

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

-- Generator for a list without duplicates
genListNoDuplicates :: Gen [Integer]
genListNoDuplicates = nub <$> listOf arbitrary

-- Generator for a list with at least one duplicate
genListWithDuplicates :: Gen [Integer]
genListWithDuplicates = do
  xs <- listOf1 arbitrary
  dup <- elements xs  -- Pick an element to duplicate
  shuffle (dup : xs)  -- Shuffle to place the duplicate at a random position

-- Property: `pnoDuplicates` should return True for a list without duplicates
prop_noDuplicatesTrueForUniqueList :: Property
prop_noDuplicatesTrueForUniqueList = forAll genListNoDuplicates $ \xs ->
  -- Convert the Haskell list to a Plutarch list
  let pxs = pconstant @(PBuiltinList PInteger) xs
      result = pnoDuplicates # pxs
  in result === pconstant True

-- Property: `pnoDuplicates` should return False for a list with duplicates
prop_noDuplicatesFalseForDuplicateList :: Property
prop_noDuplicatesFalseForDuplicateList = forAll genListWithDuplicates $ \xs ->
  let pxs = pconstant @(PBuiltinList PInteger) xs
      result = pnoDuplicates # pxs
  in  result === pconstant False

pnoDuplicatesProperties :: TestTree
pnoDuplicatesProperties = testGroup "pnoDuplicates Properties"
  [ testProperty "True for unique list" prop_noDuplicatesTrueForUniqueList
  , testProperty "False for duplicate list" prop_noDuplicatesFalseForDuplicateList
  ]
