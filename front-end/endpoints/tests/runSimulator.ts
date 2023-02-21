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
		seedPhrase:
			"accuse total squeeze adapt stand slam joy lamp poet party pear level sunny banana boil glue because either shine come drum stand shoulder damp",
		address:
			"addr_test1qzk8up9zntq4l08r53p7gmpsgc3jdg9fwk79wreqqpsm2r97fdk28qxaf96cj78p5dmupfqe8jrcfxjqrc4hectl93xqr2levc",
	}, // 100
	account2: {
		seedPhrase:
			"machine drip toe west mimic tissue fuel you audit almost segment ridge suffer wood diet priority reflect gadget crack weather course vast alpha minute",
		address:
			"addr_test1qqy4xaqqeqk8x08dghtyp32hdn40w3ph93k8erg6lt9qtquxdjv0eq7qec7p0vjux3cx3gclxnl4clpqwehzvxqgpjlsxd6uwq",
	}, // 100
	account3: {
		seedPhrase:
			"foster lunar steel trim echo blame emotion siege scissors problem audit slender soup daughter soap world symptom behind high lawsuit squeeze employ path rebuild",
		address:
			"addr_test1qp70hfk0dtqef3y77e7weduhes5qn9dyjflxsdtkanzttmkherm3zelk4cty8xnmpfx9k3hjmxsy3d3sn43469x79dsqd8azq2",
	}, // 100
	account11: {
		seedPhrase:
			"uniform monster match glimpse supply glide term load whip hard fee wrong behind curtain nephew lesson predict appear pink vendor doctor visit quality glory",
		address:
			"addr_test1qrs8llu8zcqst2dldrvh84f5jphnxjxxscmel4tes6jtm3jcfcs3snyd0tdklx8n9qnkdnvy7v2q20qxawsqsekny4gsuen575",
	}, // 100
	account12: {
		seedPhrase:
			"ugly thunder nut horn canvas common library this force solid winter guide spirit sure wagon vault bus lens mercy install wife club priority original",
		address:
			"addr_test1qz52xndt25hvu8evdshep4puv6jvvh9wmsfcm53x903rpw853z7tyfhplvl8gjq4hgpgcecmj0x2vfe2ppgkwwc7wejsqyv62y",
	}, // 100
	account13: {
		seedPhrase:
			"fire iron mirror accuse glass pact retire lava tongue vague clap combine solve prefer raven attract lens tape unable brave stock loyal okay try",
		address:
			"addr_test1qz6z9pah4v4legh2a6kren7pfl9fnclklvngg5jf8rrt0jjfpqpd6q2h56urmzl9uljzta5pwqfre5zuzckd9plyzf8qrjs37p",
	},
};

// Accounts generated with utils.generateAddressSeedPhrase()
// These account have StakingCredential
//TODO: generate new addresses
const user = {
	account1: {
		seedPhrase:
			"click path wonder art cage duty infant shy split rookie extend first unaware boat group provide exercise leopard size mammal monitor bamboo dilemma grow",
		address:
			"addr_test1qze80dj5wtpnnrflfjag7hdyn235vwqskhm0fr98kqhey4wwatz92rau7w5ny8lzctkc2tnhfxwvp9gze5yh8j957t6s6ejca0",
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
		bridgeTokenName: "",
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
	const burnAmount = -12;
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
