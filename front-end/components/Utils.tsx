import * as utils from "@/endpoints/utils";
import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";
import { deployments } from "../endpoints/test/runSimulator";
import Alert from "./Alert";
import Button from "./Button";

interface Props {
	lucid: Lucid;
}

export const Utils = ({ lucid }: Props) => {
	const [error, setError] = useState("");

	useEffect(() => {
		if (!error) return;

		const timeout = setTimeout(() => setError(""), 5000);

		return () => {
			clearTimeout(timeout);
		};
	}, [error]);

	const handleClick1 = async () => {
		const result = await utils.getAllDatums(
			lucid,
			deployments.scripts.guardianValidator
		);
		console.log(result);
	};

	const handleClick2 = async () => {
		const result = await utils.getValidDatums(
			lucid,
			deployments.scripts.guardianValidator
		);
		console.log(result);
	};

	const handleClick3 = async () => {
		const result = await utils.generateAddressSeedPhrase(lucid);
		console.log(result);
	};

	const handleClick4 = async () => {
		const result1 = lucid.utils.getAddressDetails(
			"addr_test1vzdekzrwlc0qnrnqck58edn3wyevzd3tasl0c5sx6gzsvyqxt6pfs"
		);
		const result2 = lucid.utils.getAddressDetails(
			"addr_test1vzd0jhkjnzj9jju7m93n377v8zhpy23d8e5esxn9lvravwgauckcr"
		);
		const result3 = lucid.utils.getAddressDetails(
			"addr_test1vzy73swp6dq5jepsq3hn0j7xafdfqqj8lgga73qe6g34vcq4f7hq5"
		);

		console.log(result1, result2, result3);
	};

	return (
		<div>
			<h1 className="text-5xl font-bold text-center">Utils</h1>
			<div className="flex justify-center">
				<Button onClick={() => handleClick1()} text="Get All Datums" />
				<Button onClick={() => handleClick2()} text="Get Valid Datums" />
				<Button onClick={() => handleClick3()} text="Generate Address" />
				<Button onClick={() => handleClick4()} text="Get Address Details" />
			</div>
			<Alert message={error} />
		</div>
	);
};
