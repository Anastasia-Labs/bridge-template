import initLucid from "@/utils/lucid";
import { useStoreState } from "@/utils/store";
import { Lucid } from "lucid-cardano";
import { useEffect, useState } from "react";
import { Utils } from "./Utils";
import { User } from "./User";
import { Simulator } from "./Simulator";
import { Emulator } from "./Emulator";

const MainDApp = () => {
	const walletStore = useStoreState((state: any) => state.wallet);
	const [lucid, setLucid] = useState<Lucid>();
	const [whiteListed, setWhiteListed] = useState(false);

	const isWhiteListed = async () => {
		const result = await window.cardano[
			walletStore.name.toLowerCase()
		]?.isEnabled();
		result ? setWhiteListed(true) : setWhiteListed(false);
	};

	useEffect(() => {
		isWhiteListed();
		console.log("whiteListed: ", whiteListed);
		if (whiteListed && walletStore.connected && !lucid) {
			console.log("initialize lucid");
			initLucid(walletStore.name).then((Lucid: Lucid) => {
				setLucid(Lucid);
			});
		}
	}, [lucid, walletStore.connected, whiteListed]);

	if (!lucid) return null;

	return (
		<div className="flex-column">
			<User lucid={lucid} />
			<div className="divider"></div>
			<Utils lucid={lucid} />
			<div className="divider"></div>
			<Simulator lucid={lucid} />
			<div className="divider"></div>
			<Emulator lucid={lucid} />
		</div>
	);
};

export default MainDApp;
