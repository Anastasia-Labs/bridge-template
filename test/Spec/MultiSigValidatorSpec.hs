-- MultiSigValidatorSpec module, designed for testing the MultiSig Validator smart contract functionality.
-- The module includes setup for test data and specifies test cases
-- to validate the contract's behavior under various conditions.

module Spec.MultiSigValidatorSpec (
  unitTest,
  pnoDuplicatesProperties,
  psignedByAMajorityProperties,
)
where

import MultiSigValidator (
  MultisigDatum (..),
  MultisigRedeemer (..),
  pnoDuplicates,
  psignedByAMajority,
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
  PubKeyHash (..),
  ScriptContext,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree, testGroup)

import Data.ByteString.Char8 (pack)
import Data.List (nub)
import Plutarch.Api.V2 (PPubKeyHash)
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (fromPFun)
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty.QuickCheck (Gen, Property, arbitrary, choose, elements, forAll, listOf, listOf1, shuffle, suchThat, testProperty, vectorOf)

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
  xs <- listOf1 arbitrary `suchThat` \l -> not $ null l
  dup <- elements xs -- Pick an element to duplicate
  shuffle (dup : xs) -- Shuffle to place the duplicate at a random position

-- Property: `pnoDuplicates` should return True for a list without duplicates
prop_noDuplicatesTrueForUniqueList :: Property
prop_noDuplicatesTrueForUniqueList = forAll genListNoDuplicates $ \xs ->
  fromPFun $ pnoDuplicates # pconstant @(PBuiltinList PInteger) xs

-- Property: `pnoDuplicates` should return False for a list with duplicates
prop_noDuplicatesFalseForDuplicateList :: Property
prop_noDuplicatesFalseForDuplicateList = forAll genListWithDuplicates $ \xs ->
  fromPFun $ pnot #$ pnoDuplicates # pconstant @(PBuiltinList PInteger) xs

pnoDuplicatesProperties :: TestTree
pnoDuplicatesProperties =
  testGroup
    "pnoDuplicates Properties"
    [ testProperty "True for unique list" prop_noDuplicatesTrueForUniqueList
    , testProperty "False for duplicate list" prop_noDuplicatesFalseForDuplicateList
    ]

-- Generator for a PubKeyHash based on a subset of characters
genPubKeyHash :: Gen PubKeyHash
genPubKeyHash = do
  member <- vectorOf 32 $ elements (['a' .. 'f'] ++ ['0' .. '9']) -- Hexadecimal representation
  return $ PubKeyHash . toBuiltin . pack $ member

-- Function to convert a Haskell list of PubKeyHash to a Plutarch list of PAsData PPubKeyHash
toPlutarchList :: [PubKeyHash] -> Term s (PBuiltinList (PAsData PPubKeyHash))
toPlutarchList = foldr (\x -> (#) (pcons # pdata (pconstant x))) pnil

-- Generator for a required count
genRequiredCount :: [a] -> Gen Integer
genRequiredCount signers = choose (1, toInteger $ length signers - 1)

-- Custom generator to create a non-empty sublist from a given list
sublistOf1 :: [a] -> Gen [a]
sublistOf1 [] = return [] -- If the list is empty, return an empty list (shouldn't happen with non-empty input list)
sublistOf1 lst = do
  -- Generate a non-empty sublist by ensuring at least one element is always selected
  indices <- listOf1 $ choose (0, length lst - 1) -- Choose at least one index
  return [lst !! i | i <- indices] -- Return unique elements corresponding to the chosen indices

-- Adjusted generator for allKeys and signers
genAllKeysAndSigners :: Gen ([PubKeyHash], [PubKeyHash], Integer)
genAllKeysAndSigners = do
  allKeysHaskell <- listOf1 genPubKeyHash -- Generate a Haskell list of PubKeyHash
  signersHaskell <- sublistOf1 allKeysHaskell -- Generate a sublist for signers
  requiredCount <- genRequiredCount signersHaskell -- Generate required count based on the Haskell list of signers
  return (allKeysHaskell, signersHaskell, requiredCount)

-- Property: `psignedByAMajority` should return True when the number of valid signers meets or exceeds the required count
prop_signedByMajorityTrue :: Property
prop_signedByMajorityTrue = forAll genAllKeysAndSigners $ \(allKeys, signers, requiredCount) ->
  fromPFun $ psignedByAMajority # toPlutarchList allKeys # pconstant requiredCount # toPlutarchList signers

-- Property: `psignedByAMajority` should return False when the number of valid signers is less than the required count
prop_signedByMajorityFalse :: Property
prop_signedByMajorityFalse = forAll genAllKeysAndSigners $ \(allKeys, signers, _) ->
  let validSignersCount = toInteger $ length signers + 1 -- Ensure requiredCount is not met
   in fromPFun $ pnot #$ psignedByAMajority # toPlutarchList allKeys # pconstant validSignersCount # toPlutarchList signers

psignedByAMajorityProperties :: TestTree
psignedByAMajorityProperties =
  testGroup
    "psignedByAMajority Properties"
    [ testProperty "True when valid signers meet or exceed required count" prop_signedByMajorityTrue
    , testProperty "False when valid signers are less than required count" prop_signedByMajorityFalse
    ]
