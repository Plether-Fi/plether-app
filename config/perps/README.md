# Arbitrum Sepolia perps release

`arbitrum-sepolia-v3.json` selects the replacement plether-core v1.3.0 deployment
from [Core PR #111](https://github.com/Plether-Fi/plether-core/pull/111). The active
frontend registry, public AA manifest, backend embedded manifest, worker runtime
hashes, Docker build inputs, Terraform bindings, and deployment guards all target
this graph. All 29 contract addresses and runtime hashes come from source commit
`cf86189bcca4813f39d6405a338b77176783eb42` at L2 blocks 318367917–318367988.

The exact upstream manifest is retained under `evidence/v1.3.0/manifest.json`.
`arbitrum-sepolia-v3.provenance.json` records its SHA-256 and the immutable build
archive checksum, pinned to release-record commit
`c9b121702e7228259706e1229a0300605efa8e89`. The application manifest adds source
provenance and indexing metadata without altering the upstream deployment state.
The first full volume-history minute is 1791796620, derived from deployment block
timestamp 1791796569 and hash
`0x953aff238e5fad547e638ae2abb997b0380a4993ecc071d40655597f996db99d`.

This source update replaces the app's protocol target. It does not deploy app
services or broadcast bootstrap/activation transactions. At the recorded release
checkpoint, contracts are deployed/inactive, both tranches are unseeded, and the
guardian is unset. Contract bootstrap and activation remain operator actions.
Old positions, orders, balances, claims, and vault obligations do not migrate
automatically; preserve the old deployment's servicing and signed-operation
reconciliation. Never mix deployment identities in an existing competition.
The v1.2.3 manifest and its post-deployment evidence remain historical records.

## Consumer interfaces

The app consumes V3 requests with a submission deadline and a separate execution
window. AA manifest transport identity retains its `-v2` suffix, with the fresh
20261012 date and explicit `orderInterfaceVersion: 3`. The backend calldata
policy is `bounded-v3`. The pinned SDK is built from the deployed Core revision;
registry publication is independent of this checked-in artifact.

Frontend ABI subsets and protection-worker ABIs come from the checksum-verified
v1.3.0 `build.tar.gz`. Lifecycle receipts include close economics, commitment,
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
