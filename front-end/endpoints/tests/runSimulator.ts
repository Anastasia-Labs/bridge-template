import { Lucid } from "lucid-cardano";
import * as multisig_update from "../multisig.update";
import * as multisig_deploy from "../multisig.deploy";
import * as multisig_fullfill from "../multisig.fullfill";
import {
	ConfigDeploy,
	ConfigFullFill,
	ConfigUpdateMultiSig,
} from "../types";
import * as utils from "../utils";
import * as user_request from "../user.request";
import * as user_burn from "../user.burn";
import { deployments } from "../config.deployment";

// Accounts generated with utils.generateAddressSeedPhrase()
// These account have StakingCredential
//TODO: generate new addresses
const signers = {
	account1: {
		"seedPhrase": "skate brand slam wild mandate heart pear amateur fun slim trade human few budget narrow ketchup inject twenty real scrub universe credit company weather",
		"address": "addr_test1qqhrq24gqc3h32tgpuu4qra35fc00hnxlpgzxkalnztspypk9r2y02k9p3e8ppvkap2dhwwdpsy0vuw2epds9ks9qcrs4chlcv"
	}, // 100
	account2: {
		"seedPhrase": "kiwi also magnet slow produce grocery snap twelve sense orphan settle viable math cart struggle enter pumpkin phrase census kingdom conduct furnace sock indicate",
		"address": "addr_test1qqwmxeh3m74gn96qlwelpe6vv4a5cwf0fnhtam7pvxashdcq5vqwypzque3gpzwjz9pvgqaacx6uxpch5zvhjejar0aslqm5ky"
	}, // 100
	account3: {
		"seedPhrase": "fix throw field maximum come husband midnight title hurry effort tribe dice state popular hood admit fruit fancy escape seven seminar monitor edge seat",
		"address": "addr_test1qp09add6x38jra9v4ds2e8jghxxv7tddjzrupfccq9tmnyfla6m9efnctxpr2puduz0vze3wmycvnlweuna72srtz8kscx0cz0"
	}
	, // 100
	account11: {
		"seedPhrase": "arrange couch steak capable other fee behind bitter napkin company enough street risk token betray sword tragic oxygen bag abandon dose million puppy tooth",
		"address": "addr_test1qzqh5kd70uaqejtfl0rp358tnq2thhr4p6v36uytd5vfv2y2hlhjrn285k4dz0v2te74jszsvsgjcaf7a6nwyeyu5p4qqm9q5t"
	}, // 100
	account12: {
		"seedPhrase": "grocery title weather describe issue spray group sell hurdle unfold spell puppy together witness uncle carpet property piano defense people hospital employ hedgehog wait",
		"address": "addr_test1qzmjwzml7w9f5m9wa0gqaavxzpj2zjz4k4njp4wtknjcyrg704nf5ntez8hyyp2yjw539sk5y4zy8293phn42kpddjmq3yl00s"
	}, // 100
	account13: {
		"seedPhrase": "comfort main buyer rhythm glow wink evidence gap yard clump prepare ghost mandate sunset keep elegant mass victory fox skate merry lady melody degree",
		"address": "addr_test1qrl55qj5w82zrh4ewyw0hrkjr4lfa5je7j8hfldxcmklkun5vmvmtj6wl74q236ja9nt03mjspj5hkjhp49edu70g2wqrx4n50"
	}, //100
};

// Accounts generated with utils.generateAddressSeedPhrase()
// These account have StakingCredential
const user = {
	account1: {
		"seedPhrase": "puzzle mouse invest develop donor popular absent else discover crisp always nuclear toy sting trigger wheel east health into energy again nerve spring buyer",
		"address": "addr_test1qq7wyy8jpzr39unkaal4n5pp2fy046mndst8gkvpaq8rpgj27qvf8nlk7640flpq6rydke3n0mgnuve4fqgj5ul8xuhq3zg8p5"
	},
};

// Only run this once to mint multisig nft and set datum with cosigners at multisig script
export const deploy = async (lucid: Lucid) => {
	const deployConfig: ConfigDeploy = {
		multisig: {
			requiredCount: 1,
			keys: [lucid.utils.paymentCredentialOf(signers.account1.address).hash],
		},
		bridgeTokenName: "BridgeToken",
	};

	console.log(deployConfig);
	//TODO: consider adding script addresses
	const deployments = await multisig_deploy.submit(lucid, deployConfig);

	console.log(deployments);
};

export const update = async (lucid: Lucid) => {
	const configUpdate: ConfigUpdateMultiSig = {
		unit: deployments.units.multiSigCert,
		multiSigValidator: deployments.scripts.multiSigValidator,
		oldKeys: [lucid.utils.paymentCredentialOf(signers.account1.address).hash],
		newConfig: {
			requiredCount: 2,
			keys: [
				lucid.utils.paymentCredentialOf(signers.account1.address).hash,
				lucid.utils.paymentCredentialOf(signers.account2.address).hash,
				lucid.utils.paymentCredentialOf(signers.account3.address).hash,
			],
		},
	};
	console.log(configUpdate);
	lucid.selectWalletFromSeed(signers.account1.seedPhrase);

	const updateTx = await multisig_update.build(lucid, configUpdate);

	lucid.selectWalletFromSeed(signers.account1.seedPhrase);
	const witness1 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	// lucid.selectWalletFromSeed(signers.account2.seedPhrase);
	// const witness2 = await multisig_update.signWitness(
	// 	lucid,
	// 	updateTx.toString()
	// );

	// lucid.selectWalletFromSeed(signers.account3.seedPhrase);
	// const witness3 = await multisig_update.signWitness(
	// 	lucid,
	// 	updateTx.toString()
	// );

	const assembleTx = await multisig_update.assemble(
		lucid,
		updateTx.toString(),
		// [witness1,witness2, witness3]
		[witness1]
	);

	console.log(assembleTx);
};

// Fullfill requests from users
export const fullfil = async (lucid: Lucid) => {
	const configSign: ConfigFullFill = {
		units: deployments.units,
		scripts: deployments.scripts,
		keys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};

	lucid.selectWalletFromSeed(signers.account1.seedPhrase);

	// Get Valid Datums from Guardian Script
	const validDatumUtxoList = await utils.getValidDatums(
		lucid,
		deployments.scripts.guardianValidator
	);
	if (!validDatumUtxoList?.length) {
		console.log("No valid datums at Guardian Script");
		return null;
	}
	console.log("validDatumUtxoList: ", validDatumUtxoList);

	// Build transaction with Valid Datums and UTXOs
	// Guardian Minter, Guardian Script and Guardian Multisig are inlcuded
	const fulfillTx = await multisig_fullfill.build(
		lucid,
		[validDatumUtxoList[0]],
		configSign
	);

	lucid.selectWalletFromSeed(signers.account1.seedPhrase);
	const witness1 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromSeed(signers.account2.seedPhrase);
	const witness2 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromSeed(signers.account3.seedPhrase);
	const witness3 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	const assembleTx = await multisig_fullfill.assemble(
		lucid,
		fulfillTx.toString(),
		[witness1, witness2, witness3]
	);

	console.log(assembleTx);
};

export const request = async (lucid: Lucid) => {
	lucid.selectWalletFromSeed(user.account1.seedPhrase);

	// This Address has Staking Credential
	const myAddress = await lucid.wallet.address();
	const bridgeAmount = 10;
	const otherChainAddress = "15U6C9gZs5G3i11gTfmhqCaKK6V7bqGdmi";
	console.log(`Requesting ${bridgeAmount} BridgeToken to ${myAddress}`);
	const result = await user_request.submit(
		lucid,
		bridgeAmount,
		myAddress,
		otherChainAddress,
		deployments.scripts.guardianValidator
	);
	console.log(result);
};

export const burn = async (lucid: Lucid) => {
	lucid.selectWalletFromSeed(user.account1.seedPhrase);
	const burnAmount = -5;
	const otherChainAddress = "15U6C9gZs5G3i11gTfmhqCaKK6V7bqGdmi";
	const result = await user_burn.submit(
		lucid,
		burnAmount,
		otherChainAddress,
		deployments.scripts.wrapMintingPolicy,
		deployments.units.bridgeToken,
	);
	console.log(result);
};
