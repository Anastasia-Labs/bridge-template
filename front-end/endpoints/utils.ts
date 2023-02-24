import * as validators from "@/utils/validators";
import {
	Lucid,
	Data,
	Address,
	Credential,
	generatePrivateKey,
	generateSeedPhrase,
	Script,
	MintingPolicy,
	applyParamsToScript,
	KeyHash,
	TxHash,
	Constr,
	SpendingValidator,
	toText,
	fromText,
} from "lucid-cardano";
import { AnyDatumUTXO, ValidDatumUTXO, DeployedScripts } from "./types";

// TODO: WIP update function
export const getAllDatums = async (
	lucid: Lucid,
	guardianValApplied: Script
): Promise<AnyDatumUTXO[]> => {
	console.log("Getting All Datums");
	const guardianValidatorAddr: Address =
		lucid.utils.validatorToAddress(guardianValApplied);

	const scriptUtxos = await lucid.utxosAt(guardianValidatorAddr);
	if (!scriptUtxos.length) return [] as AnyDatumUTXO[];

	const datumUtxoList = scriptUtxos.map((utxo) => {
		const datumCbor = utxo.datum || "";
		const datumAsData: any = Data.from(datumCbor);
		// Try parsing Data -> Address
		// Address: must have StakingHash
		// Valid Address type:  (PubKeyCredential (<PubKeyHash>)) (Just (StakingHash (PubKeyCredential (<PubKeyHash>))))
		const bridgeAmount = datumAsData.fields[0];
		const btcAddress = toText(datumAsData.fields[1]);
		const paymentCredentialHash: string =
			datumAsData.fields[2]?.fields[0]?.fields[0];
		const stakeCredentialHash: string =
			datumAsData.fields[2]?.fields[1]?.fields[0]?.fields[0]?.fields[0];

		if (
			!paymentCredentialHash ||
			!stakeCredentialHash ||
			!bridgeAmount ||
			!btcAddress
		) {
			return {
				isValid: false,
				datum: datumAsData,
				utxo: utxo,
			};
		}

		const paymentCredential: Credential = lucid.utils.keyHashToCredential(
			paymentCredentialHash
		);

		const stakeCredential: Credential =
			lucid.utils.keyHashToCredential(stakeCredentialHash);

		const readableDatum = {
			bridgeAmount: bridgeAmount,
			btcAddress: btcAddress,
			cardanoAddress: lucid.utils.credentialToAddress(
				paymentCredential,
				stakeCredential
			), // Convert to Bech32 Address
		};

		return {
			isValid: true,
			datum: readableDatum,
			utxo: utxo,
		};
	});
	return datumUtxoList;
};

//TODO: Maybe consider using TypeBox or Zod for safety data validation
export const getValidDatums = async (
	lucid: Lucid,
	guardianValApplied: Script
): Promise<ValidDatumUTXO[]> => {
	const guardianValidatorAddr: Address =
		lucid.utils.validatorToAddress(guardianValApplied);

	const scriptUtxos = await lucid.utxosAt(guardianValidatorAddr);
	if (!scriptUtxos.length) return [] as ValidDatumUTXO[];

	const datumUtxoList = scriptUtxos.reduce((acc: ValidDatumUTXO[], utxo) => {
		const datumCbor = utxo.datum || "";
		const datumAsData: any = Data.from(datumCbor);

		const bridgeAmount = datumAsData.fields[0];
		const otherChainAddress = toText(datumAsData.fields[1]);
		const paymentCredentialHash: string =
			datumAsData.fields[2]?.fields[0]?.fields[0];
		// return staking credential hash if exits, otherwise returns undefined
		const stakeCredentialHash: string =
			datumAsData.fields[2]?.fields[1]?.fields[0]?.fields[0]?.fields[0];

		//Only allow stakeCredentialHash with hash or undefined
		if (
			paymentCredentialHash &&
			bridgeAmount &&
			otherChainAddress &&
			stakeCredentialHash !== ""
		) {
			const paymentCredential: Credential = lucid.utils.keyHashToCredential(
				paymentCredentialHash
			);

			const stakeCredential: Credential =
				lucid.utils.keyHashToCredential(stakeCredentialHash);

			const cardanoAddress = stakeCredentialHash
				? lucid.utils.credentialToAddress(paymentCredential, stakeCredential)
				: lucid.utils.credentialToAddress(paymentCredential);

			const readableDatum = {
				bridgeAmount: bridgeAmount,
				otherChainAddress: otherChainAddress,
				cardanoAddress: cardanoAddress,
			};
			const newdata = {
				datum: readableDatum,
				utxo: utxo,
			};

			acc.push(newdata);
		}
		return acc;
	}, []);
	return datumUtxoList;
};

// Only use this if you want to create new hardcoded accounts in prepod, then these accounts must be funded from your wallet
export const generateAddressPrivateKey = async (lucid: Lucid) => {
	const privKey = generatePrivateKey();
	const address = await lucid
		.selectWalletFromPrivateKey(privKey)
		.wallet.address();

	return {
		privateKey: privKey,
		address: address,
	};
};

// Only use this if you want to create new hardcoded accounts in prepod, then these accounts must be funded from your wallet
export const generateAddressSeedPhrase = async (lucid: Lucid) => {
	const seedPhrase = generateSeedPhrase();
	const address = await lucid.selectWalletFromSeed(seedPhrase).wallet.address();

	return {
		seedPhrase: seedPhrase,
		address: address,
	};
};

export const buildScripts = (
	lucid: Lucid,
	key: KeyHash,
	txHash: TxHash,
	outputIndex: number,
	bridgeTokenName: string
) => {
	const multiSigMintingPolicy: MintingPolicy = {
		type: "PlutusV2",
		script: applyParamsToScript(validators.multiSigMintingPolicy.script, [
			key, // (PAsData PPubKeyHash)
			lucid.utils.validatorToScriptHash(validators.multiSigValidator), // (PAsData PScriptHash)
			new Constr(0, [new Constr(0, [txHash]), BigInt(outputIndex)]), // PTxOutRef
		]),
	};

	const guardianValidator: SpendingValidator = {
		type: "PlutusV2",
		script: applyParamsToScript(validators.guardianValidator.script, [
			lucid.utils.validatorToScriptHash(validators.multiSigValidator), // (PAsData PScriptHash)
			lucid.utils.mintingPolicyToId(multiSigMintingPolicy), // (PAsData PCurrencySymbol)
		]),
	};
	const wrapMintingPolicy: MintingPolicy = {
		type: "PlutusV2",
		script: applyParamsToScript(validators.wrapMintingPolicy.script, [
			fromText(bridgeTokenName), // (PAsData PTokenName)
			lucid.utils.validatorToScriptHash(guardianValidator), // (PAsData PScriptHash)
		]),
	};

	return {
		multiSigValidator: validators.multiSigValidator,
		multiSigMintingPolicy: multiSigMintingPolicy,
		guardianValidator: guardianValidator,
		wrapMintingPolicy: wrapMintingPolicy,
	} as DeployedScripts;
};
