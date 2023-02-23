import { Data, KeyHash, Script, TxHash, Unit, UTxO } from "lucid-cardano";

export type ValidDatumUTXO = {
	datum: { bridgeAmount: bigint; cardanoAddress: string; otherChainAddress: string };
	utxo: UTxO;
};

export type AnyDatumUTXO = {
	isValid: boolean;
	datum: { amountDeposit: bigint; address: string } | Data;
	utxo: UTxO;
};

export type ConfigMultiSig = {
	keys: KeyHash[];
	requiredCount: number;
};

export type ConfigDeploy = {
	multisig: ConfigMultiSig;
	bridgeTokenName: string;
}

export type ConfigUpdateMultiSig = {
	multiSigValidator: Script;
	unit: Unit;
	oldKeys: KeyHash[];
	newConfig: ConfigMultiSig;
};

export type ConfigFullFill = {
	units: {
		multiSigCert: Unit;
		bridgeToken: Unit;
	};
	scripts: DeployedScripts;
	keys: KeyHash[];
};

export type DeployedScripts = {
	multiSigValidator: Script;
	multiSigMintingPolicy: Script;
	guardianValidator: Script;
	wrapMintingPolicy: Script;
};

export type Deployments = {
	txHash: TxHash;
	scripts: DeployedScripts;
	units: {
		multiSigCert: Unit;
		bridgeToken: Unit;
	};
};
