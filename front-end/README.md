## Getting Started

Install packages:
```
npm install
```

Create `.env.local` file as follows:

Preprod
```
BLOCKFROST_KEY=<project-key>
API_URL="https://cardano-preprod.blockfrost.io/api/v0"
NETWORK="Preprod"
```
or

Mainnet
```
BLOCKFROST_KEY=<project-key>
API_URL="https://cardano-mainnet.blockfrost.io/api/v0"
NETWORK="Mainnet"
```

run the development server:

```bash
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.
