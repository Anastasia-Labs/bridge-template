import { Constr, Data, fromText, Lucid, toUnit } from "lucid-cardano";
import { ConfigDeploy } from "./types";
import { buildScripts } from "./utils";

export const submit = async (lucid: Lucid, config: ConfigDeploy) => {
	const walletUtxos = await lucid.wallet.getUtxos();
	const walletTxHash = walletUtxos[0].txHash;
	const walletOutputIndex = walletUtxos[0].outputIndex;

	const scripts = buildScripts(
		lucid,
		config.multisig.keys[0],
		walletTxHash,
		walletOutputIndex
	);
	const multisigValidatorAddr = lucid.utils.validatorToAddress(
		scripts.multiSigValidator
	);

	const multisigPolicyId = lucid.utils.mintingPolicyToId(
		scripts.multiSigMintingPolicy
	);

	const wrapPolicyId = lucid.utils.mintingPolicyToId(scripts.wrapMintingPolicy);

	const units = {
		multiSigCert: toUnit(multisigPolicyId, fromText("MultiSigCert")),
		bridgeToken: toUnit(wrapPolicyId, fromText(config.bridgeTokenName)),
	};
	const asset = { [units.multiSigCert]: BigInt(1) };

	const Datum = Data.to(
		new Constr(0, [config.multisig.keys, BigInt(config.multisig.requiredCount)])
	);

	const RedeemerPolicy = Data.to(new Constr(0, [])); // PMintGuardianCrt

	const tx = await lucid
		.newTx()
		.collectFrom([walletUtxos[0]])
		.attachMintingPolicy(scripts.multiSigMintingPolicy)
		.mintAssets(asset, RedeemerPolicy)
		.payToContract(multisigValidatorAddr, { inline: Datum }, asset)
		.complete();

	const signedTx = await tx.sign().complete();
	const txHash = await signedTx.submit();
	return { txHash, scripts, units };
};
