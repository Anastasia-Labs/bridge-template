module Spec.WrapMintPolicySpec (
  unitTest,
) where

import GuardianValidator (WitnessDatum (..))
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
  ScriptContext,
  ScriptHash,
  TokenName (..),
  Value,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)
import WrapMintPolicy (MintBTCAction (..), policy)

scriptHash :: ScriptHash
scriptHash = "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e"

currencySymbol :: CurrencySymbol
currencySymbol = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

tokenName :: TokenName
tokenName = "token"

mintedValue :: Value
mintedValue = singleton currencySymbol tokenName 1

burntValue :: Value
burntValue = singleton currencySymbol tokenName (-1)

multisigValAddress :: Address
multisigValAddress =
  Address
    (ScriptCredential "22e380f1157b688ac08f26a64e046b8b85632ba47c664c8f924b777e")
    Nothing

adaAddress :: Address
adaAddress =
  Address
    (PubKeyCredential "f26a64e046b8b85632ba47c664c8f924b777e22e380f1157b688ac08")
    Nothing

inputUTXO :: UTXO
inputUTXO =
  mconcat
    [ address multisigValAddress
    , withValue (singleton "" "" 4_000_000)
    , withRefTxId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
    , withRefIndex 1
    , withInlineDatum $
        WitnessDatum
          { bridgeAmt = 1
          , otherChainAddr = "94907d6b58e7587ff7212ae40117d0a868fe89e7af0f95e52a437594"
          , cardanoPKH = adaAddress
          }
    ]

outputUTXO :: UTXO
outputUTXO =
  mconcat
    [ address adaAddress
    , withValue (singleton "" "" 9_000_000 <> mintedValue)
    ]

wrongMintedValue :: Value
wrongMintedValue =
  singleton currencySymbol tokenName 2

goodMintCtx :: ScriptContext
goodMintCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , mint mintedValue
      , withMinting currencySymbol
      ]

wrongMintAmountCtx :: ScriptContext
wrongMintAmountCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , mint wrongMintedValue
      , withMinting currencySymbol
      ]

wrongAddressOutputUTXO :: UTXO
wrongAddressOutputUTXO =
  mconcat
    [ address
        ( Address
            (PubKeyCredential "c664c8f924b777e22e380f1157b688ac08f26a64e046b8b85632ba47")
            Nothing
        )
    , withValue (singleton "" "" 9_000_000 <> mintedValue)
    ]

wrongOutputAddressMintCtx :: ScriptContext
wrongOutputAddressMintCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , output wrongAddressOutputUTXO
      , mint mintedValue
      , withMinting currencySymbol
      ]

goodBurnCtx :: ScriptContext
goodBurnCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , mint burntValue
      , withMinting currencySymbol
      ]

wrongBurnCtx :: ScriptContext
wrongBurnCtx =
  buildMinting' $
    mconcat
      [ input inputUTXO
      , mint mintedValue
      , withMinting currencySymbol
      ]

unitTest :: TestTree
unitTest = tryFromPTerm "Wrap Mint Policy Unit Test" (policy # pdata (pconstant tokenName) # pdata (pconstant scriptHash)) $ do
  testEvalCase
    "Pass - Burn Policy"
    Success
    [ PlutusTx.toData BurnBTC
    , PlutusTx.toData goodBurnCtx
    ]
  testEvalCase
    "Failure - Burn Policy"
    Failure
    [ PlutusTx.toData BurnBTC
    , PlutusTx.toData wrongBurnCtx
    ]
  testEvalCase
    "Success - Mint"
    Success
    [ PlutusTx.toData MintBTC
    , PlutusTx.toData goodMintCtx
    ]
  testEvalCase
    "Failure - Mint Wrong Value"
    Failure
    [ PlutusTx.toData MintBTC
    , PlutusTx.toData wrongMintAmountCtx
    ]
  testEvalCase
    "Failure - Mint Wrong Output Address"
    Failure
    [ PlutusTx.toData MintBTC
    , PlutusTx.toData wrongOutputAddressMintCtx
    ]
