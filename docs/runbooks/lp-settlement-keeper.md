# LP Epoch Settlement Keeper (Arbitrum Sepolia)

This runbook activates the v1.2.0 vault epoch-settlement worker embedded in
`plether-keeper`. It is Sepolia-only. Mainnet must remain in `off` mode.

The worker settles contract queues; it does not enumerate depositors or
withdrawers and never calls claim or refund functions. HousePool preserves the
protocol order: Senior withdrawals, Junior withdrawals, Junior deposits, then
Senior deposits.

## Safety model

`LP_SETTLEMENT_MODE` has three states:

- `off`: no LP worker is started.
- `observe`: bindings, health, payload selection, affordability, and the exact
  transaction simulation run, but the transaction is never signed or sent.
- `execute`: signed intents are committed to PostgreSQL before broadcast and
  are recovered or replaced at the same nonce after restart.

In `observe` and `execute`, `LP_SETTLEMENT_POLL_SECONDS` must be exactly 15.
Startup rejects any other active polling interval.

The LP worker has its own signer, database connection, advisory lock, and
thread. Order execution cannot serialize an LP pass. Every pass uses a
confirmation-pinned Settlement Monitor observation and fails closed unless its
schema, epoch, block, health, dependency, fault, blocker, maturity, and route
invariants all hold.

The reviewed v1.2.0 facade reports onchain configuration and observation
schema version `4`; that deployed value is compiled into the keeper's decoder.

## Initial off deployment

1. Apply the additive database and application release with
   `lp_settlement_mode = "off"`.
2. Confirm the normal consolidated worker is healthy. Do not enable the legacy
   `LP_SETTLEMENT_ENABLED`; `true` is intentionally a startup error.
3. Generate a dedicated secp256k1 key. It must differ from the order,
   liquidation, and oracle-updater keys. While the mode remains `off`, apply
   Terraform with the key so it is stored only in the Terraform-managed
   `/plether/sepolia/lp-settlement-private-key` SecureString and appears in the
   newly registered keeper task definitions.
4. Configure a working operations alarm SNS topic and keep
   `workers_desired_count = 1` with `consolidate_workers = true`.
5. Fund the signer with enough Sepolia ETH to enter observe mode. Preflight
   rejects a zero balance; the long-lived worker remains able to start and
   reconcile durable receipts when its balance is low, but it fails
   affordability before creating new work and emits the low-balance signal.
6. Run the read-only preflight below successfully with that funded signer.
   Only after both funding and a successful preflight may
   `lp_settlement_signer_funding_confirmed = true` be set for the observe
   rollout; the Terraform flag is an operator attestation, not a funding step.

## Read-only preflight

Run the deployed image as a separate one-shot process with the same database,
RPC, environment, and secrets as the live keeper:

```bash
plether-keeper --lp-settlement-preflight
```

The command does not acquire either long-lived keeper lock and does not mutate
the database. It verifies the PostgreSQL LP schema, RPC chain ID, deployed
bytecode, monitor schema and bindings, HousePool, Router, Engine, both vaults,
the dedicated signer, and its balance. When safe matured work exists, it builds
and simulates the exact cached or six-feed atomic transaction.

Require exit code zero and `lp_settlement_preflight_succeeded`. Treat
`lp_settlement_invariant_failure` as a stop condition; do not bypass it by
changing addresses or using a different preflight signer.

The long-lived keeper audits chain ID, bytecode, bindings, and signer before it
launches its workers. A failure alerts and blocks all new signing or recovery
sends. The process remains alive so an already-persisted nonce lane can still
reconcile canonical receipts; the standalone preflight remains the hard
rollout gate for schema, balance, and exact simulation.

## Observe soak and cost budget

1. Apply `lp_settlement_mode = "observe"` and deploy the backend from `master`
   through the GitHub CLI workflow described in `AGENTS.md`.
2. Queue controlled Senior and Junior deposits and withdrawals before an hourly
   boundary.
3. Soak through a complete epoch. Require one-minute
   `lp_settlement_heartbeat` events and at least one
   `lp_settlement_observe_would_submit` for eligible work, with no
   `lp_settlement_broadcast` event.
4. Exercise or observe both cached-mark and atomic-oracle routes. Record the
   largest `maximum_transaction_cost_wei` value. Set
   `lp_settlement_max_tx_cost_wei` to twice that maximum.
5. Fund the signer to at least eight times the configured cap (the default
   four-transaction drain budget with 2x reserve). This eight-cap reserve is
   fixed even if the runtime drain limit is temporarily configured below four.

If only one route occurred during the soak, remain in observe until the other
route has a reviewed simulation; do not infer its cost from the cached path.

## Execute activation

1. Repeat preflight with the final cap and signer funding.
2. Change only `lp_settlement_mode` to `execute`, apply Terraform, and deploy
   away from an hourly boundary.
3. At the next eligible boundary, require a first confirmed settlement within
   two minutes while the monitor remains complete, healthy, and unblocked.
4. Match each success to exactly one configured-HousePool `LpEpochSettled`
   event. Confirm its cutoff epoch, four funded/deposited amounts, backlog
   flags, and deferred-entry flag are present in the durable transaction row.
5. Verify deposit shares become claimable and withdrawal USDC becomes
   claimable or remains FIFO queued. A cycle may immediately confirm at most
   four transactions; a later 15-second cycle can continue the backlog.

## Pending transactions and manual review

The durable nonce lane is authoritative. After 30 seconds the worker may
rebroadcast identical raw bytes. After 60 seconds it may replace at the same
nonce with higher fees, up to three replacements and never above the cost cap.
It waits for `KEEPER_CONFIRMATIONS` and returns a reorged receipt to pending.

Do not submit a later nonce manually while a row is active. If the confirmed
nonce is consumed without a receipt belonging to the persisted same-nonce
family, or if receipt/event/hash invariants fail, the lane enters
`manual_review`. Investigate all family hashes, canonical receipts, and the
signer's pending/latest nonce before changing database state. Preserve the
broadcast history; it is append-only by design.

The following alarms page operators:

- heartbeat absent for three minutes;
- continuously safe/ready backlog for five minutes;
- a pending transaction older than two minutes or at replacement cap;
- binding, schema, receipt, event, or unexpected-revert invariants;
- signer balance below twice the four-transaction cost budget.

## Rollback

Set the mode to `observe` and redeploy. The worker continues reconciling an
already persisted transaction and recording a receipt, but it does not
rebroadcast, replace, sign, or submit new work. Keep the signer funded and the
worker running until the nonce lane is resolved. Use `off` only after no active
transaction remains and receipt/audit data has been retained.
