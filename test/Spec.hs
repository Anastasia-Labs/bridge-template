module Main (main) where

import Spec.GuardianValidatorSpec qualified
import Spec.MultiSigMintPolicySpec qualified
import Spec.MultiSigValidatorSpec qualified
import Spec.WrapMintPolicySpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit Test Group"
      [ Spec.GuardianValidatorSpec.unitTest
      , Spec.MultiSigMintPolicySpec.unitTest
      , Spec.MultiSigValidatorSpec.unitTest
      , Spec.MultiSigValidatorSpec.pnoDuplicatesProperties
      , Spec.WrapMintPolicySpec.unitTest
      ]
