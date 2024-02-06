# Bridge Template Test Suite Documentation

This document provides an overview of the test suites included in the Bridge Template project. Each test suite is designed to validate different components of the project, ensuring that they meet the required specifications and function as expected.

## Running the Tests

To run the tests, use the following command:

```sh
cabal new-test --test-show-details=streaming
```

This command will compile and execute all the test suites defined in the project. The output will show the status of each test case.

## Test Suites Overview

The test suites are organized into different groups, each targeting specific components or functionalities of the Bridge Template. Below is a detailed description of each test suite and its test cases.

### Unit Test Group

#### Guardian Validator Unit Test

- **Pass - Validation**: Verifies that the Guardian Validator correctly validates transactions under valid conditions.
-**Failure - Invalid multisig input UTXO**: Ensures the validator rejects transactions with invalid multisig input UTXOs.
-**Failure - Invalid multisig output UTXO**: Ensures the validator rejects transactions with invalid multisig output UTXOs.

#### MultiSig Mint Policy Unit Test

-**Pass - Policy**: Validates that the MultiSig Mint Policy operates correctly under intended conditions.
-**Failure - Invalid output UTXO**: Checks the policy rejects invalid output UTXOs.
-**Failure - Wrong minted value**: Verifies the policy rejects transactions with incorrect minted values.
-**Failure - Wrong burnt value**: Ensures the policy rejects transactions with incorrect burnt values.
-**Failure - Wrong output datum**: Confirms the policy rejects transactions with incorrect output data.
-**Failure - Wrong count output datum**: Validates the policy rejects transactions with incorrect counts of output data.

#### MultiSig Validator Unit Test

-**Pass - Update Validation**: Tests successful validation of updates.
-**Failure - Update Validation - miss signatories**: Checks rejection of updates missing required signatories.
-**Failure - Update Validation - invalid new required count**: Ensures rejection of updates with invalid new required counts.
-**Failure - Update Validation - invalid new key count**: Validates rejection of updates with invalid new key counts.
-**Pass - Sign Validation**: Tests successful validation of signatures.
-**Failure - Sign Validation - miss signatories**: Ensures rejection of signatures missing required signatories.
-**Failure - Sign Validation - wrong output datum**: Checks rejection of transactions with incorrect output data.

#### Wrap Mint Policy Unit Test

-**Pass - Burn Policy**: Validates the correct operation of the Burn Policy.
-**Failure - Burn Policy**: Ensures the policy rejects transactions that do not meet the burn criteria.
-**Success - Mint**: Validates successful minting operations.
-**Failure - Mint Wrong Value**: Ensures rejection of minting operations with incorrect values.
-**Failure - Mint Wrong Output Address**: Checks rejection of minting operations with incorrect output addresses.

### Test Results

Upon running the tests, you will receive a summary of the test results indicating the status of each test case within the suites. A test case marked as OK has passed successfully, while any failures or errors will be detailed in the output.

```markdown
Test suite bridge-template-test: RUNNING...
Unit Test Group
  Guardian Validator Unit Test
    Pass - Validation:                                        OK (0.02s)
    Failure - Invalid multisig input UTXO:                    OK
    Failure - Invalid multisig output UTXO:                   OK
  MultiSig Mint Policy Unit Test
    Pass - Policy:                                            OK (0.01s)
    Failure - Invalid output UTXO:                            OK
    Failure - Wrong minted value:                             OK
    Failure - Wrong burnt value:                              OK
    Failure - Wrong output datum:                             OK
    Failure - Wrong count output datum:                       OK
  MultiSig Validator Unit Test
    Pass - Update Validation:                                 OK (0.08s)
    Failure - Update Validation - miss signatories:           OK
    Failure - Update Validation - invalid new required count: OK
    Failure - Update Validation - invalid new key count:      OK
    Pass - Sign Validation:                                   OK
    Failure - Sign Validation - miss signatories:             OK
    Failure - Sign Validation - wrong output datum:           OK
  Wrap Mint Policy Unit Test
    Pass - Burn Policy:                                       OK (0.26s)
    Failure - Burn Policy:                                    OK
    Success - Mint:                                           OK
    Failure - Mint Wrong Value:                               OK
    Failure - Mint Wrong Output Address:                      OK

All 21 tests passed (0.38s)
Test suite bridge-template-test: PASS
```
