# Arbitrum Sepolia perps release configuration

`arbitrum-sepolia-v2.0.0.json` is the current perps consumer configuration. It
selects Core v2.0.0 on Arbitrum Sepolia (421614), canonical schema 4 and order
interface 3, using six-decimal MockUSDG. All 30 addresses and runtime hashes come
from the published release manifest, including the new `cfdEnginePlanningReader`.

The frontend registry, backend embedded manifest, workers, Docker and deployment
guards select this file. `infra/terraform/sepolia-core-v2.0.0.tfvars.json` contains
the matching backend/vault bindings and deployment block 323658447. Volume starts
at the first whole minute after that block: 1793185200 (2026-10-28 11:00 UTC).
Terraform defaults remain unbound; applying the overlay is an operator step.

The original release asset is preserved byte-for-byte in
`evidence/v2.0.0/manifest.json`; its SHA-256 matches GitHub's release digest.
`consumer-bundle.json` hashes the app's ABI, SDK and deployment evidence. Its
checksum is the app release's `bundleSha256`; it is an app evidence inventory,
not an upstream compiler archive. The runtime verification records read-only
checks of all 30 contracts at one safe block. ABI provenance separately identifies
compiled source and does not claim bytecode reproduction.

The public AA manifest binds this market with a fresh version. Sponsorship is
false and its paymaster remains null: Core's release does not deploy AA
infrastructure. See [the integration runbook](../../docs/runbooks/perps-mock-usdg.md)
for the remaining database, worker and AA rollout steps. This source update does
not deploy services, apply Terraform, migrate databases or enable sponsorship.

Current token metadata is `settlementToken`; the token pin is
`contracts.settlementToken`. Quantities use `Assets` and `AssetAtoms`, AA metadata
uses `settlementToken*`, and environment/Terraform use `PERPS_SETTLEMENT_TOKEN` /
`perps_settlement_token`. MockUSDG supports mint/approve/transfer/deposit without
signed-token authorization or bridge funding. Rewards are not applicable.

## Historical evidence

`mock-usdg-prepared.json` is retained as an inactive parser/safety fixture.
`arbitrum-sepolia-v1.json`, `arbitrum-sepolia-v2.json`,
`arbitrum-sepolia-v3.json` and `evidence/v1.3.1` retain their original deployments.
Historical Terraform overlays, SDK provenance and signed-operation evidence are
not fallback configurations for current consumers.
