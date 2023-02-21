import { useState, useEffect } from 'react';
import { useStoreActions, useStoreState } from "../utils/store";
import initLucid from "../utils/lucid";
import Image from 'next/image'

const WalletConnect = () => {
    const walletStore = useStoreState(state => state.wallet)
    const setWallet = useStoreActions(actions => actions.setWallet)
    const availableWallets = useStoreState(state => state.availableWallets)
    const setAvailableWallets = useStoreActions(actions => actions.setAvailableWallets)

    const [connectedAddress, setConnectedAddress] = useState("")
    const [mounted, setMounted] = useState(false);

    const loadWalletSession = async () => {
        const result = await window.cardano[walletStore.name.toLowerCase()]?.isEnabled()
        result ? walletConnected(walletStore.name) : setWallet({ connected: false, name: '', address: '' })
    }

    const walletConnected = async (wallet: string) => {
        const addr = await (await initLucid(wallet)).wallet.address()
        const walletStoreObj = { connected: true, name: wallet, address: addr }
        setConnectedAddress(addr)
        setWallet(walletStoreObj)
    }

    const selectWallet = async (wallet: string) => {
        if (
            window.cardano &&
            (await window.cardano[wallet.toLocaleLowerCase()].enable())
        ) {
            await walletConnected(wallet)
        }
    }

    useEffect(() => {
        if (window.cardano) {
            const walletList = Object.keys(window.cardano).filter((walletName) =>
                window.cardano[walletName].icon &&
                walletName !== "ccvault" &&
                walletName !== "typhon"
            )
            setAvailableWallets(walletList)
            loadWalletSession()
            setMounted(true)
        }
    }, [])

    return mounted ? (
        <>
            <div className="dropdown dropdown-end">
                <label tabIndex={0} className="btn m-1">{connectedAddress != "" ? 'Connected' : 'Connect'}</label>
                <ul tabIndex={0} className="dropdown-content menu p-2 shadow bg-base-300 rounded-box w-52">
                    {availableWallets.map((wallet) =>
                        <li key={wallet} onClick={() => { selectWallet(wallet) }} >
                            <div className="flex flex-row justify-evenly">
                                <div className="basis-8">
                                    <Image src={window.cardano[wallet].icon} alt={''} width={100} height={100}/>
                                </div>
                                <div className="basis-1"> {wallet.charAt(0).toUpperCase() + wallet.slice(1)}</div>
                            </div>
                        </li>
                    )}
                </ul>
            </div>
        </>
    ) : null
}

export default WalletConnect;