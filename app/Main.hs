module Main (main) where

import GuardianValidator qualified
import MultiSigMintPolicy qualified
import MultiSigValidator qualified
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
 )
import Utils (writePlutusScript)
import WrapMintPolicy qualified

main :: IO ()
main = do
  exist <- doesDirectoryExist "compiled"
  createDirectoryIfMissing exist "compiled"

  writePlutusScript
    "Multisig Validator"
    "./compiled/multisigValidator.plutus"
    MultiSigValidator.validator

  writePlutusScript
    "Multisig Minting Policy"
    "./compiled/multisigMintingPolicy.plutus"
    MultiSigMintPolicy.policy

  writePlutusScript
    "Guardian Validator"
    "./compiled/guardianValidator.plutus"
    GuardianValidator.validator

  writePlutusScript
    "CBTC Minting Policy"
    "./compiled/wrapMintingPolicy.plutus"
    WrapMintPolicy.policy
