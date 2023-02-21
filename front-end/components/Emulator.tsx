import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";
import * as runEmulator from "@/endpoints/test/runEmulator";
import Alert from "./Alert";
import Button from "./Button";

interface Props {
	lucid: Lucid;
}

export const Emulator = ({ lucid }: Props) => {
	const [error, setError] = useState("");

	useEffect(() => {
		if (!error) return;

		const timeout = setTimeout(() => setError(""), 5000);

		return () => {
			clearTimeout(timeout);
		};
	}, [error]);

	const handleClick1 = async () => {
		//TODO: Code pending
		// await runEmulator.request(lucid);
	};

	const handleClick2 = async () => {
		//TODO: Code pending
		// await runEmulator.fullfil(lucid);
	};

	const handleClick3 = async () => {
		await runEmulator.update();
	};

	const handleClick4 = async () => {
		// Code pending
		// await runEmulator.init(lucid);
	};

	return (
		<div>
			<h1 className="text-5xl font-bold text-center">Emulator</h1>
			<div className="flex justify-center">
				<Button onClick={() => handleClick1()} text="Request(TODO)" />
				<Button onClick={() => handleClick2()} text="FullFill(TODO)" />
				<Button onClick={() => handleClick3()} text="Update" />
				<Button onClick={() => handleClick4()} text="Deploy(TODO)" />
			</div>
			<Alert message={error} />
		</div>
	);
};
