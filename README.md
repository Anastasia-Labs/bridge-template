
# Table of Contents

- [Introduction](#introduction)
- [Documentation](#documentation)
  - [GuardianValidator Module](#guardianvalidator-module)
  - [MultiSigMintPolicy Module](#multisigmintpolicy-module)
  - [MultiSigValidator Module](#multisigvalidator-module)
  - [WrapMintPolicy Module](#wrapmintpolicy-module)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Building and Developing](#building-and-developing)
  - [Testing](#testing)

# Introduction

In the rapidly evolving world of blockchain technology, the need for seamless communication and interoperability between disparate blockchain networks has never been more critical. The Bridge Template serves as a pivotal solution in this domain, designed to bridge the gap between different blockchain ecosystems. This innovative template enables the efficient and secure transfer of assets and data across various networks, breaking down the barriers that have traditionally hindered interoperability in the blockchain space.

At its core, the Bridge Template is engineered to facilitate a harmonious flow of information and value between distinct blockchains. Whether it's transferring tokens, exchanging data, or executing cross-chain smart contracts, this template ensures that these processes are not only possible but are executed with the utmost security, speed, and reliability.

# Documentation

## GuardianValidator Module

The `GuardianValidator` module serves as a crucial checkpoint in the transaction process. It receives and assesses requests, determining their eligibility based on specific criteria.

- **Request Reception**: Accepts transaction requests for processing.
- **Certificate Verification**: Upon fulfillment of a request, it verifies if the transaction holds the required certificate, ensuring compliance with set standards and protocols.

## MultiSigMintPolicy Module

The `MultiSigMintPolicy` module is pivotal in managing the minting aspect of multisignature transactions. It is responsible for issuing minting certificates, which are essential for transactions involving the multisig Validator.

- **Certificate Issuance for Minting**: Generates certificates required for minting operations, facilitating the approval process in multisignature setups.
- **Key and Signature Management**: Manages keys and monitors the count of required signatures, ensuring that all minting actions are collectively authorized.

## MultiSigValidator Module

The `MultiSigValidator` module plays a key role in ensuring the integrity of transactions originating from the `GuardianValidator`. It verifies that all transactions have the necessary number of signatures before proceeding.

- **Signature Verification**: Checks that each request has an adequate number of valid signatures to meet the threshold for execution.
- **Transaction Authentication**: Guarantees that only fully authorized transactions are executed, maintaining security and trust in the multisig process.

## WrapMintPolicy Module

The `WrapMintPolicy` module is tasked with the controlled minting and burning of tokens in response to requests validated by the `GuardianValidator`.

- **Token Lifecycle Management**: Oversees the minting (creation) and burning (destruction) of tokens as dictated by validated requests.
- **Alignment with Guardian Requests**: Ensures all mint and burn actions are in strict accordance with the parameters defined in the guardian validator requests.

Each module within this system contributes to a secure, efficient, and compliant framework for handling complex multisignature transactions on the Cardano blockchain, reinforcing the robustness and flexibility of blockchain operations.

## Transaction

### Deploy multisig validator

This step involves the initialization of the multisig validator. It includes minting Multisig Certification tokens and transferring them to the multisig validator address. Alongside the tokens, a datum is sent containing a list of public key hashes of the validators and the required number of signatures to authorize a multisig transaction.
![deploy.png](/assets/images/deploy.png)

### User request
Users initiate requests to the guardian validator. These requests include a datum that specifies the number of tokens to be wrapped or burned, the target address on the other blockchain, and the user's own Cardano address. This process is the primary interface for users to interact with the multisig system for cross-chain transactions.

![request.png](/assets/images/request.png)

### Multisig fulfill

In this phase, the multisig system aggregates requests from the guardian validator. It utilizes the certification tokens in the multisig validator to mint or burn bridge tokens as specified in the user requests. This step ensures that the user's cross-chain transaction requests are processed and fulfilled accurately.
![fulfill.png](/assets/images/fulfill.png)


# Getting Started

## Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org/download.html) installed on your system. Nix is used for package management and to provide a consistent development environment.
To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.18.1
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes ca-derivations
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=
```

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target you can use with the `nix develop` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

## Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/bridge-template.git
```

Navigate to the repository directory:

```sh
cd bridge-template
direnv allow
```

Activate the development environment with Nix:

```sh
nix develop .
```

Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
cabal build all
```

Execute the test suite:

```sh
cabal test
```

![plutarch-merkle-tree.gif](/assets/images/bridge-template.gif)

## Demeter Workspace 

To provide a seamless experience for running and trying out our application, click the workspace button below. This will start a workspace in Demeter with our repository code automatically cloned.


[![Code in Workspace](https://demeter.run/code/badge.svg)](https://demeter.run/code?repository=https://github.com/Anastasia-Labs/bridge-template&template=haskell&size=large) 

## Testing

For comprehensive information on testing the Bridge Template implementation, including unit tests and property-based tests, please refer to our [test documentation](/test/README.md).
