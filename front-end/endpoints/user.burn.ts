import { Constr, Lucid, Data, Script, toUnit, fromText, Unit } from "lucid-cardano";

export const submit = async (
	lucid: Lucid,
	burnAmount: number,
	returnAddress: string,
	wrapMintingPolicy: Script,
	bridgeToken: Unit,
) => {
	try {
		
		const walletUtxos = await lucid.utxosAtWithUnit(
			await lucid.wallet.address(),
			bridgeToken
		);
		const redeemer = Data.to(new Constr(1, []));

		const totalAssets = { [bridgeToken]: BigInt(burnAmount) };
		const tx = await lucid
			.newTx()
			.collectFrom(walletUtxos)
			.attachMintingPolicy(wrapMintingPolicy)
			.mintAssets(totalAssets, redeemer)
			.attachMetadata(0, { returnAddress: returnAddress, burnedAmount: burnAmount })
			.complete();

		const signedTx = await tx.sign().complete();

		const txHash = signedTx.submit();
		return txHash;
	} catch (error) {
		if (error instanceof Error) return error;
		return Error(`error : ${JSON.stringify(error)}`);
	}
};
