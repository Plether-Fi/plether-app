# Perps bridge funding

This module funds a pinned destination Trading Account with Ethereum USDC or USDT through Across. The connected wallet sends ordinary Ethereum transactions and needs ETH for approvals and the bridge transaction. Changing the wallet network does not change the beneficiary; changing the wallet owner selects a different persisted funding state.

The feature is disabled unless `VITE_PERPS_FUNDING_MANIFEST_URL` names a valid same-origin JSON release and the backend enables the matching configuration. There is no default mainnet deployment, testnet bridge, or inferred token route. Existing saved intents remain accessible while new funding is disabled, provided their reviewed release remains available. Live use still requires an authenticated provider quote proving the exact destination recipe and a matching deployed clearinghouse release; the generated fixtures below do not satisfy either requirement or prove compatibility with another clearinghouse version.

The funding release contains `destinationChainId`, `releaseId`, `clearinghouse`, `clearinghouseCodeHash`, `token`, `confirmations`, and `startBlock`, plus these six provider pins:

- `multicallHandler` and `multicallHandlerCodeHash`
- `destinationSpokePool` and `destinationSpokePoolCodeHash`
- `destinationSpokePoolImplementation` and `destinationSpokePoolImplementationCodeHash`

Additional artifact metadata and deployment `evidence` are allowed. The destination must be Arbitrum One (42161), with native USDC at `0xaf88d065e77c8cc2239327c5edb3a432268e5831`. New intents also require the active `VITE_PERPS_DEPLOYMENT_JSON` release and the resolved AA manifest to match. The separate release validator and backend verify deployment evidence and live bindings; a JSON file alone is not a deployment attestation. Old receiver/factory releases and saved quotes lack the required fields and cannot authorize a new transfer.

`api.ts` uses the same-origin `/api/perps/funding` API. Quotes use a bytes32 `quoteId`, integer Unix-second `expiresAt`, decimal-string amounts, immutable owner/beneficiary/destination fields, the exact `destinationMessage`, and ordered `sourceTransactions`. Intents use `intentId`. The worker proxy requires its own `PERPS_FUNDING_BACKEND_URL`; it never falls back to the Sepolia backend. Provider credentials stay on the backend.

Before quote review and again before requesting source-wallet transactions, `deployment.ts` reads the destination chain independently of the connected wallet. At one block it checks the handler, SpokePool proxy and implementation, clearinghouse, and reviewed event emitter runtime hashes; verifies the proxy EIP-1967 implementation slot; and checks the clearinghouse settlement token. Missing or failed RPC proof blocks funding.

Before requesting signatures, `provider.ts` decodes the supported source SpokePool or periphery call and checks its source amount, refund owner, destination token, chain, shared handler recipient, output bound, and message. `destinationActions.ts` requires a canonical ABI message whose fallback recipient is the Trading Account beneficiary and whose first four calls are exactly:

1. Ask the handler to approve the clearinghouse for the handler's current USDC balance.
2. Ask the handler to call `depositFor(beneficiary, balance)` on the clearinghouse.
3. Clear the handler's USDC allowance to the clearinghouse.
4. Emit the unique quote ID through the reviewed inert event emitter.

The first two calls use `makeCallWithBalance` with zero amount placeholders and byte offset 36. The deployed handler ORs its token balance into that word, so nonzero placeholders are rejected. It consumes its full current USDC balance, which can include unsolicited dust. Every call has zero native value. Only an optional bounded suffix of two USDC drains to the same beneficiary and two inert metadata events is accepted. Source approvals are limited to the exact quoted amount, preserving a zero-reset approval when needed. Unsupported calls, targets, routes, or fallback recipients are rejected. No receiver deployment, factory, application signing worker, or receiver recovery transaction is needed.

If a destination action fails, the reviewed handler rolls back the inner actions and returns USDC to the beneficiary. Only canonical backend evidence for this specific bridge fill can establish `needs-deposit`, including the fallback transaction, block identity, and returned amount. This is historical evidence of a return, not evidence that the tokens remain unspent or that margin was credited. The UI refreshes account data and offers the existing Deposit flow using only the original Trading Account's current USDC balance. It rechecks owner, beneficiary, chain, and active release before submission. The ordinary owner-wallet deposit path is unchanged.

A browser submission lock and fresh storage read prevent concurrent tabs from submitting the same pending intent. The app persists an ambiguous-send marker before the bridge wallet request and its hash before API registration. Interrupted requests are never automatically repeated. A manually entered replacement hash is persisted only after backend verification of the same reviewed Ethereum call. This recovery remains available if the original hash was saved locally but registration failed. Completed references are archived locally before another transfer begins. An explicit archive of a returned transfer requires a fresh canonical fallback proof and preserves its history; it does not establish margin credit. A source failure can be archived only after a fresh canonical reverted receipt with matching hash and block identity and at least two confirmations.

Only a fresh backend `confirmed` response containing the clearinghouse deposit transaction, canonical block identity, and credited amount meeting the minimum can display **Ready to trade**. Provider `filled`, a handler/account token balance, fallback delivery, and saved confirmations are insufficient. Unavailable canonical reads or retracted evidence remove readiness. The backend must correlate this exact source deposit, destination fill, message, quote marker, and clearinghouse credit, and reconcile reorganizations. Provider `expired` and `refunded` hints remain advisory. Preserve the funding reference and source, deposit, or fallback transaction hashes when investigating an interrupted transfer.

`destinationActions.fixture.json` is a generated viem ABI fixture shared with the backend tests, not a live provider quote. `acrossQuote.fixture.json` preserves a historical public USDT source-swap payload; provider tests replace its obsolete receiver destination with the generated direct-action message. `acrossRuntime.fixture.json` contains the verified inert emitter runtime used by deployment-pin tests. These fixtures do not establish successful live quoting, transaction simulation, a deployed funding release, or a completed transfer. Handler success, rollback, and fallback behavior are separately exercised against deployed bytecode by the core contract tests.

Run the focused checks from `apps/frontend`:

```sh
npx vitest run --configLoader runner --project unit src/perps-funding
npx eslint src/perps-funding
```
