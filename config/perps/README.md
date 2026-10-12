# Arbitrum Sepolia perps release

`arbitrum-sepolia-v3.json` selects the replacement plether-core v1.3.1 deployment
from [Core PR #112](https://github.com/Plether-Fi/plether-core/pull/112). The active
frontend registry, public AA manifest, backend embedded manifest, worker runtime
hashes, Docker build inputs, Terraform bindings, and deployment guards all target
this graph. All 29 contract addresses and runtime hashes come from source commit
`cf86189bcca4813f39d6405a338b77176783eb42` at L2 blocks 318398363–318398657.

The exact upstream manifest is retained under `evidence/v1.3.1/manifest.json`.
`arbitrum-sepolia-v3.provenance.json` records its SHA-256 and the immutable build
archive checksum, pinned to release-record commit
`22f4b70ba97ebed98a2c821958caf25313771faf`. The application manifest adds source
provenance and indexing metadata without altering the upstream deployment state.
The first full volume-history minute is 1791804360, derived from deployment block
timestamp 1791804328 and hash
`0x6f47679ab4d34b543ce1099a6a67cdcf2fea90f79daf01f69e15e25218af9d9e`.

This source update replaces the app's protocol target. It does not deploy app
services or broadcast bootstrap/activation transactions. At the recorded release
checkpoint, contracts are trading active, Senior is permanently seeded with
10M mock USDC, Junior with 500M, and the guardian is unset.
Old positions, orders, balances, claims, and vault obligations do not migrate
automatically; preserve the old deployment's servicing and signed-operation
reconciliation. Never mix deployment identities in an existing competition.
The v1.2.3 and v1.3.0 manifests remain historical records.

## Consumer interfaces

The app consumes V3 requests with a submission deadline and a separate execution
window. AA manifest transport identity retains its `-v2` suffix, with the fresh
20261012 date, `v131` deployment marker and explicit `orderInterfaceVersion: 3`. The backend calldata
policy is `bounded-v3`. The pinned SDK is built from the deployed Core revision;
registry publication is independent of this checked-in artifact.

Frontend ABI subsets and protection-worker ABIs come from the checksum-verified
v1.3.0 `build.tar.gz`, reused unchanged by v1.3.1. Lifecycle receipts include close economics, commitment,
bounty disposition, and timing. The V5 book's `terminalOutcome` stores a compact
summary; full UI execution evidence comes from the canonical `OrderFinalized`
event. Canonical indexing supplies economics omitted by the compact summary.
Historical order execution quotes `getOrderExecutionFee(bytes[])`, while mark
refreshes retain `getUpdateFee(bytes[])`.

The close preview is deployed with this graph and uses the request-taking
`previewClose` overload. A successful prospective preview validates bounty
backing from free settlement and eligible position pledge. Closes do not require
an additional free-settlement deposit or a subsidy batch. New subsidy issuance
is disabled; the September sponsored-close lens remains archived with its old
engine/router bindings. Bridge funding remains independently gated.

Regenerate protection-worker ABIs with:

```sh
node scripts/generate-protection-worker-abi.mjs /path/to/build.tar.gz
```

The backend embeds `arbitrum-sepolia-v3.json` at compile time. Cabal tracks it as a
build dependency, and Docker copies the same file. Runtime environment bindings
must match the embedded release and deployment anchor. Rebuild consumers after
changing the release file; do not duplicate deployed addresses in Haskell code.
