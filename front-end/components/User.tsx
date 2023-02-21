import { deployments } from "@/endpoints/tests/runSimulator";
import * as user_request from "@/endpoints/user.request";
import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";
import Alert from "./Alert";
import Button from "./Button";

interface Props {
	lucid: Lucid;
}

export const User = ({ lucid }: Props) => {
	const [error, setError] = useState("");

	useEffect(() => {
		if (!error) return;

		const timeout = setTimeout(() => setError(""), 5000);

		return () => {
			clearTimeout(timeout);
		};
	}, [error]);

	const handleClick = async () => {
		const myAddress = await lucid.wallet.address();
		const hardcodedAmount = 10;
		const result = await user_request.submit(
			lucid,
			hardcodedAmount,
			myAddress,
			"",
			deployments.scripts.guardianValidator
		);
		if (result instanceof Error) {
			setError(result.message);
		} else {
			console.log(result);
		}
	};

	return (
		<div>
			<h1 className="text-5xl font-bold text-center">User</h1>
			<div className="flex justify-center">
				<Button onClick={() => handleClick()} text="Submit Request" />
			</div>
			<Alert message={error} />
		</div>
	);
};
