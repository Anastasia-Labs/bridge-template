import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";
import * as runEmulator from "@/endpoints/tests/runEmulator";
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

	const handleClick3 = async () => {
		await runEmulator.run();
	};

	return (
		<div>
			<h1 className="text-5xl font-bold text-center">Emulator</h1>
			<div className="flex justify-center">
				<Button onClick={() => handleClick3()} text="Run" />
			</div>
			<Alert message={error} />
		</div>
	);
};
