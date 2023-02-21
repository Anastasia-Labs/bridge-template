import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";
import * as runSimulator from "@/endpoints/tests/runSimulator";
import Alert from "./Alert";
import Button from "./Button";

interface Props {
	lucid: Lucid;
}

export const Simulator = ({ lucid }: Props) => {
	const [error, setError] = useState("");

	useEffect(() => {
		if (!error) return;

		const timeout = setTimeout(() => setError(""), 5000);

		return () => {
			clearTimeout(timeout);
		};
	}, [error]);

	const handleClick1 = async () => {
		await runSimulator.request(lucid);
	};

	const handleClick2 = async () => {
		await runSimulator.fullfil(lucid);
	};

	const handleClick3 = async () => {
		await runSimulator.update(lucid);
	};

	const handleClick4 = async () => {
		await runSimulator.deploy(lucid);
	};

	const handleClick5 = async () => {
		await runSimulator.burn(lucid);
	};

	return (
		<div>
			<h1 className="text-5xl font-bold text-center">Simulator</h1>
			<div className="flex justify-center">
				<Button onClick={() => handleClick1()} text="Request" />
				<Button onClick={() => handleClick5()} text="Burn" />
				<Button onClick={() => handleClick2()} text="FullFill" />
				<Button onClick={() => handleClick3()} text="Update" />
				<Button onClick={() => handleClick4()} text="Deploy" />
			</div>
			<Alert message={error} />
		</div>
	);
};
