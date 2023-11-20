module Main (main, mainEval1, mainEval2) where

import Test.Tasty (
  defaultMain,
  testGroup,
 )

import Data.Text.IO qualified as TIO
import Spec.GuardianValidatorSpec qualified as GuardianValidatorSpec
import Spec.MintWrapSpec qualified as MintWrapSpec
import Utils (evalT)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit Test"
      [ MintWrapSpec.sampleTest
      , GuardianValidatorSpec.sampleTest
      ]

mainEval1 :: IO ()
mainEval1 = do
  case evalT MintWrapSpec.sampleTestEval of
    Left e -> TIO.putStrLn e
    Right r -> putStrLn (show r)

mainEval2 :: IO ()
mainEval2 = do
  case evalT GuardianValidatorSpec.sampleTestEval of
    Left e -> TIO.putStrLn e
    Right r -> putStrLn (show r)
