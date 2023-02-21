import { Blockfrost, Lucid, Network } from 'lucid-cardano';

const blockfrostKey = process.env.BLOCKFROST_KEY as string
const apiURL = process.env.API_URL as string
const network = process.env.NETWORK as Network

const initLucid = async (wallet: string) => {
    const api = await window.cardano[wallet.toLowerCase()].enable()
    const lucid = await Lucid.new(new Blockfrost(apiURL, blockfrostKey), network)
    lucid.selectWallet(api)
    return lucid;
}

export default initLucid;