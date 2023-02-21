import { Constr, Data, Lucid } from "lucid-cardano";
import { ConfigUpdateMultiSig } from "./types";

export const build = async (lucid: Lucid, config: ConfigUpdateMultiSig) => {
	const scriptUtxo = await lucid.utxoByUnit(config.unit);

	const multisigValidatorAddr = lucid.utils.validatorToAddress(
		config.multiSigValidator
	);

	// const Datum = Data.to(
	// 	new Constr(0, [
	// 		config.newConfig.cosignerKeys,
	// 		BigInt(config.newConfig.threshold),
	// 	])
	// )

	const MyDatum = Data.Object({
		keys: Data.Array(Data.String),
		requiredCount: Data.BigInt,
	});
	type MyDatum = Data.Static<typeof MyDatum>;

	const datum: MyDatum = {
		keys: config.newConfig.keys,
		requiredCount: BigInt(config.newConfig.requiredCount),
	};

	const Datum = Data.to<MyDatum>(datum, MyDatum);

	const RedeemerUpdate = Data.to(new Constr(0, [])); // Update

	const signers = config.oldKeys
		.map((key) => {
			return lucid.newTx().addSignerKey(key);
		})
		.reduce((prevTx, tx) => {
			return prevTx.compose(tx);
		});

	const tx = await lucid
		.newTx()
		.collectFrom([scriptUtxo], RedeemerUpdate)
		.attachSpendingValidator(config.multiSigValidator)
		.payToContract(multisigValidatorAddr, { inline: Datum }, scriptUtxo.assets)
		.compose(signers)
		.complete();

	return tx;
};

export const signWitness = async (lucid: Lucid, txAsCbor: string) => {
	return await lucid.fromTx(txAsCbor).partialSign();
};

export const assemble = async (
	lucid: Lucid,
	txAsCbor: string,
	witnesses: string[]
) => {
	const signedTx = await lucid.fromTx(txAsCbor).assemble(witnesses).complete();
	const txHash = signedTx.submit();
	return txHash;
};
