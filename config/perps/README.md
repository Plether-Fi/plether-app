# Arbitrum Sepolia perps release configuration

`mock-usdg-prepared.json` is the only current perps consumer configuration. It
selects canonical schema 4 and order interface 3 for a future MockUSDG deployment.
Phase `prepared` and null addresses, runtime hashes, source commit and indexing
boundary mean that perps consumers and deployment guards must remain disabled.
They are not placeholders for the old deployment.

The frontend registry and backend embedded manifest use this file. Cabal tracks
it as a dependency; Docker copies the same file. Worker paths, Terraform guards
and release tooling select it as well. Populate the full graph only from reviewed
deployed evidence. See [the integration runbook](../../docs/runbooks/perps-mock-usdg.md)
for offline candidate generation and later activation requirements.

Current token metadata is `settlementToken`; the token contract pin is
`contracts.settlementToken`. Contract/SDK/API quantities use `Assets` and
`AssetAtoms`, AA metadata uses `settlementToken*`, and environment/Terraform use
`PERPS_SETTLEMENT_TOKEN` / `perps_settlement_token`. No current consumer accepts
old token keys or historical schema variants. Six-decimal MockUSDG supports
ordinary mint/approve/transfer/deposit, with no signed-token authorization or
bridge funding. Rewards are not applicable.

Current ABI consumers are generated from the breaking Core source, independently
of deployment evidence. The V5 compact terminal summary is joined to the verified
V5 `OrderFinalized` event; its receipt hash binds the event's terminal block.
The token-neutral ABI is incompatible with the former deployment despite retaining
order interface 3 and receipt/configuration V5 labels.

## Historical evidence

The `arbitrum-sepolia-v1.json`, `arbitrum-sepolia-v2.json`,
`arbitrum-sepolia-v3.json`, versioned provenance files and `evidence/` archives
remain historical records. In particular, v1.3.1 deployed source
`cf86189bcca4813f39d6405a338b77176783eb42` at L2 blocks 318398363–318398657.
Its upstream manifest checksum and release-record commit remain in
`arbitrum-sepolia-v3.provenance.json`; its public AA manifest is preserved under
`evidence/v1.3.1/perps-aa-manifest.json`. Historical Terraform overlays preserve
the exact deployment identity and are not current activation configuration.

Do not relabel historical amounts or mutate these records to look canonical.
Balances, claims, signed operations, vault obligations and competition history
do not migrate automatically between deployments.
