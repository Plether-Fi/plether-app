# Perps bridge funding

This module funds a pinned destination Trading Account with Ethereum USDC or USDT through Across, then waits for USDC to be credited by the Arbitrum clearinghouse. The connected wallet sends ordinary Ethereum transactions and needs ETH for approvals and the bridge transaction. Changing the wallet network does not change the intent's beneficiary; changing the wallet owner selects a different persisted funding state.

The feature is disabled unless `VITE_PERPS_FUNDING_MANIFEST_URL` explicitly names a valid same-origin JSON release and the backend enables the matching configuration. There is no default mainnet deployment, testnet bridge, or inferred token route. Existing saved intents remain accessible when the backend disables new funding, provided the explicit release manifest is still available.

The funding release contains `destinationChainId`, `releaseId`, `clearinghouse`, `clearinghouseCodeHash`, `token`, `receiverFactory`, `factoryCodeHash`, `confirmations`, and `startBlock`. Additional deployment `evidence` is allowed. The destination must be Arbitrum One (42161), with native USDC at `0xaf88d065e77c8cc2239327c5edb3a432268e5831`. The new-intent gate also matches the active `VITE_PERPS_DEPLOYMENT_JSON` release and the resolved AA manifest. The separate release validator and backend verify deployment evidence and live contract bindings; a JSON file alone is not a deployment attestation.

`api.ts` uses the same-origin `/api/perps/funding` API. Backend quotes use integer Unix-second `expiresAt`, canonical decimal-string amounts, immutable `intentSalt` and `ownerAddress`/beneficiary/destination fields, and an ordered `sourceTransactions` array. Intents use `intentId`. The worker proxy requires its own `PERPS_FUNDING_BACKEND_URL`; it never falls back to the Sepolia backend. Provider credentials remain on the backend.

Before quote review and again before requesting source-wallet transactions, `receiver.ts` reads the destination chain independently of the connected wallet, checks factory and clearinghouse runtime hashes, verifies factory bindings, and requires `predictReceiver(beneficiary, intentSalt)` to equal the quoted receiver. Existing receivers must expose the same beneficiary, token, and clearinghouse. Missing or failed RPC proof blocks funding.

Before requesting signatures, `provider.ts` decodes the supported Across SpokePool or periphery call and checks its source amount, refund owner, destination token, destination chain, receiver, and output bound. For the reviewed USDT route, the only accepted destination message is the four-call handler recipe: two exact-USDC drains to the receiver and two metadata events at the reviewed logger, with zero native values and zero fallback. Unsupported bridge methods, destination calls, targets, or routes are rejected. ERC-20 approvals are limited to the exact quoted source amount, preserving a zero-reset approval when required. The backend independently checks calldata and the deployed handler/logger runtime hashes.

A browser submission lock and a fresh storage read prevent concurrent tabs from submitting the same pending intent. The app persists an ambiguous-send marker before the bridge wallet request, then persists its transaction hash before reporting it to the backend. Interrupted requests are not automatically repeated. Recovery can report an existing hash or retry depositing funds already at the receiver. Source hashes entered manually are persisted only after the API accepts them. Completed references are archived locally before another deposit begins. A source failure may be archived only after a fresh backend read proves a canonical reverted receipt with a matching source hash, block identity, and at least two confirmations; an advisory failure alone is insufficient. The backend may accept a wallet replacement hash only after verifying the same reviewed bridge call on Ethereum.

Only a fresh backend `confirmed` response containing the clearinghouse deposit transaction, canonical block identity, and credited amount meeting the minimum can display **Ready to trade**. Provider `filled`, receiver balances, and saved confirmations are insufficient. Backend status retractions or unavailable canonical reads remove readiness. The backend is responsible for confirmations, matching clearinghouse event evidence, and reorg reconciliation. Provider `expired` or `refunded` observations remain advisory and never establish margin credit.

The UI offers destination-network return and explains beneficiary-only receiver recovery. It does not request arbitrary recovery transactions or claim that a failed bridge can be resent safely. Preserve the funding reference, receiver address, and source/deposit transaction hashes when investigating an interrupted transfer.

The captured `acrossQuote.fixture.json` is a public, read-only Ethereum-USDT-to-Arbitrum-USDC quote used to test decoding of the actual provider payload. It does not establish a successful transaction simulation, a deployed funding release, or a completed live transfer. Provider ABI definitions follow Across's `SpokePoolPeripheryInterface` and `MulticallHandler` sources.

Run the focused checks from `apps/frontend`:

```sh
npx vitest run --configLoader runner --project unit src/perps-funding
npx eslint src/perps-funding
```
