import { Constr, Lucid, Data, fromText, toUnit } from "lucid-cardano";
import { ConfigFullFill, ValidDatumUTXO } from "./types";

export const build = async (
	lucid: Lucid,
	guardianDatumUtxoList: ValidDatumUTXO[],
	config: ConfigFullFill
) => {
	const info = {
		multisigVal: {
			validator: config.scripts.multiSigValidator,
			utxo: await lucid.utxoByUnit(config.unit),
			address: lucid.utils.validatorToAddress(config.scripts.multiSigValidator),
			redeemer: Data.to(new Constr(1, [])), // PSign
		},
		guardianVal: {
			validator: config.scripts.guardianValidator,
			redeemer: Data.to(new Constr(0, [])), // PApproveWrap
			utxos: guardianDatumUtxoList.map((value) => {
				return value.utxo;
			}),
		},
		cBTCMint: {
			policy: config.scripts.cBTCMintingPolicy,
			policyID: lucid.utils.mintingPolicyToId(config.scripts.cBTCMintingPolicy),
			unit: toUnit(
				lucid.utils.mintingPolicyToId(config.scripts.cBTCMintingPolicy),
				fromText("cBTC")
			),
			redeemer: Data.to(new Constr(0, [])), // PMintBTC
		},
	};
	console.log("info", info);
	console.log(lucid.utils.validatorToScriptHash(info.multisigVal.validator));
	console.log(lucid.utils.validatorToScriptHash(info.guardianVal.validator));
	console.log(lucid.utils.validatorToScriptHash(info.cBTCMint.policy));

	const totalAmount = guardianDatumUtxoList.reduce((acc, value) => {
		return acc + value.datum.amountDeposit;
	}, BigInt(0));

	const totalAssets = { [info.cBTCMint.unit]: totalAmount };

	const guardianValTx = lucid
		.newTx()
		.attachSpendingValidator(info.guardianVal.validator)
		.collectFrom(info.guardianVal.utxos, info.guardianVal.redeemer);

	const multisigValTx = lucid
		.newTx()
		.attachSpendingValidator(info.multisigVal.validator)
		.collectFrom([info.multisigVal.utxo], info.multisigVal.redeemer)
		.payToContract(
			info.multisigVal.address,
			{ inline: info.multisigVal.utxo.datum || "" },
			info.multisigVal.utxo.assets
		);

	const cBTCMintTx = lucid
		.newTx()
		.attachMintingPolicy(info.cBTCMint.policy)
		.mintAssets(totalAssets, info.cBTCMint.redeemer);

	const outputs = guardianDatumUtxoList
		.map((value) => {
			return lucid.newTx().payToAddress(value.datum.address, {
				[info.cBTCMint.unit]: value.datum.amountDeposit,
			});
		})
		.reduce((prevTx, tx) => {
			return prevTx.compose(tx);
		});

	const signers = config.keys
		.map((key) => {
			return lucid.newTx().addSignerKey(key);
		})
		.reduce((prevTx, tx) => {
			return prevTx.compose(tx);
		});

	const tx = await lucid
		.newTx()
		.compose(guardianValTx)
		.compose(multisigValTx)
		.compose(cBTCMintTx)
		.compose(outputs)
		.compose(signers)
		.complete();

	// const tx = await lucid
	// 	.newTx()
	// 	.attachMintingPolicy(info.minter.policy)
	// 	.mintAssets(totalAssets, info.minter.redeemer)
	// 	.collectFrom(info.guardian.utxos, info.guardian.redeemer)
	// 	.attachSpendingValidator(info.guardian.validator)
	// 	.collectFrom([info.multisig.utxo], info.multisig.redeemer)
	// 	.attachSpendingValidator(info.multisig.validator)
	// 	.payToContract(
	// 		info.multisig.address,
	// 		{ inline: info.multisig.utxo.datum || "" },
	// 		info.multisig.utxo.assets
	// 	)
	// 	.compose(outputs)
	// 	.compose(signers)
	// 	.complete();

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
