# Worker funding and action-specific oracle readiness

Status: implemented and tested locally. Not deployed. No funding, signing, Core
configuration or Singapore changes are authorized by this implementation.

## Readiness behavior

The API reads both oracle policies at one exact block, validates current basket
reads using the corresponding `getLatestPrice(uint8)` mode, checks the six-feed
cached payload's age/divergence, then verifies that block is still canonical.
It uses the released v1.2.3 contract semantics (Core source
`ffe45937b7f38133133ad292c5435828bf99357d`).

| Action | LIVE | FAD | Frozen |
| --- | --- | --- | --- |
| Open/increase | Valid basket, payload and active trading required | Blocked by open policy | Blocked by open policy |
| Voluntary close/reduce | Close policy and current payload evidence | Same live-age rules; open-only market status does not block exits | Close policy's wider age allowance; never substitute a stored mark for a validated basket |
| Protection | Trigger payload must also meet the worker's 15-second age bound | Close/trigger rules, not open policy | New armed triggers unavailable; cancellation and latched retries remain independently usable |

Missing/malformed data or RPC failure yields `unknown`, not a claimed historical
cause or a fresh hard blocker. Current healthy evidence cannot guarantee a future
live/FAD order's unique post-commit payload, price bounds, FIFO execution or final
trade success. Those checks stay in Core and the existing worker path.

Alto funding affects sponsored actions. Keeper funding/liveness affects opens
and closes. Oracle-updater, liquidation and LP-settlement funding are displayed
as background observations: the keeper supplies the execution update fee, so an
unfunded updater alone must not prohibit a valid close. Protection configuration,
cancellation and explicit retries are not globally blocked by worker funding.
The existing readiness-enforcement switch remains the only enforcement switch.

`workers` is an optional, validated extension to readiness version 1. It exposes
only component, status and stable reason. Healthy status remains collapsed in
Trading Account activity; background warnings expand without claiming trading
is unavailable. The trade preview panel remains removed.

## Funding observer

`node /app/protection/funding-main.mjs` runs a read-only chain observer with a
dedicated database advisory lease. No private keys are provided. Each ten-second
iteration reads fixed-block balances/confirmed nonces, fresh pending nonces and
gas fees, and then rechecks the canonical header. Inventory: up to 16 distinct
dedicated signers, including every Alto executor plus keeper, oracle, liquidation,
protection and LP settlement. Utility-wallet auto-refills are not introduced.

Reserves are `gasLimit * bufferedCurrentMaxFee + valueWei`, where `gasLimit`
and `valueWei` are reviewed conservative bounds for an entire execution/batch
(including Pyth ETH update fees). Values are estimates for monitoring, not
spending caps or transaction parameters. Match each worker's deployed gas/batch/
value limits and fee buffer; re-review this inventory when those change. Never
use an observed single cheap transaction as the whole batch's maximum reserve.

Liability sources:

- LP settlement: existing signed transaction families, including replacements,
  unbroadcast prepared rows and unresolved/failed/abandoned attempts.
- Liquidation: persisted pending signer/nonce/value/gas/fee metadata.
- Protection: existing signed recovery bytes, decoded locally with chain and
  recovered signer verification. Bytes never leave that process through logs.
- Alto, keeper and oracle: visible pending transaction bodies from the configured
  RPC, matched to the exact next block's parent. Their existing transaction
  pipelines are not replaced or given a new recovery journal by this change.

For each nonce, reserve the **largest** replacement cost once. Include queued and
prepared future nonces; nonces consumed at the fixed block are already reflected
in its balance. A pending nonce gap without transaction bodies is **unknown**,
never zero liability. Providers without pending-body visibility may therefore
show uncertainty while a transaction is in flight. This is not a complete view
of private/provider-hidden mempools or Alto's not-yet-signed UserOperation queue;
paymaster UserOperation reservations remain solely in the existing AA ledger.

Warn below ten reserves. Below one conservative upper-bound reserve means
uncertain affordability, not proof an actual transaction cannot execute. A fresh
verified empty signer can be blocked. A mixed funded/empty executor pool is a
warning; it is not evidence that every executor is unusable. All-empty pools can
be blocked. Missing evidence, invalid fees, reorgs, regressed heads, stale/future
headers and bounded-query overflow remain unknown.

The observer atomically publishes its entire inventory into the additive
`aa_funding_observations` table. Inventory fingerprints prevent old signer/config
observations from being accepted after a configuration change. API observations
expire after 15 seconds, including when the observer exits or cannot persist.
No network calls happen inside its publishing database transaction. The keeper's
existing row continues to prove worker liveness; when configured, the shared
observer owns the funding assessment rather than the legacy pending-nonce rule.

## Frankfurt activation (requires deployment approval)

1. Build/test the backend and log-router images, record immutable digests. Do not
   reuse the currently deployed image pins: they predate these new files.
2. Apply `apps/backend/config/migrations/aa-funding-v1.sql` to Frankfurt only.
   API needs SELECT on the new table. Observer needs SELECT on existing recovery
   journals and SELECT/INSERT/DELETE on this advisory table, not recovery writes.
   Production database URLs retain certificate-verified TLS configuration.
3. Supply `aa_funding_monitors` with all six role categories and the actual public
   signers. Each entry has exactly `component`, `address`, `gasLimit`, `valueWei`
   and `feeBufferBps`; the last three are unsigned decimal **strings**. Multiple
   Alto executors each get their own entry. Do not copy synthetic test addresses.
   Disabled/unconfigured workers must be resolved explicitly rather than
   inventing a funded signer or describing them as healthy.
4. Set `aa_funding_monitor_image` to the tested backend digest, promote the API
   and log-router images, and review the Frankfurt-only plan. Terraform passes
   categorical components plus the inventory fingerprint to the API; the
   observer gets public inventory, DB/RPC access, and no signer secrets. The
   optional nonessential sidecar stays absent when the inventory is empty.
5. Verify all intended rows refresh while idle, then controlled pending-liability
   and low-funding cases, LIVE/FAD/frozen readiness, and a separately approved
   sponsored trade. Check diagnostics ingestion in PostHog 208816. No new funding
   allowance is implied. Keep readiness observation-only until live qualification.

The canary guard rejects partial inventories, missing image pins, nonconsolidated
workers and non-Frankfurt activation. Default/live Terraform image pins and
running services have not been changed by this implementation.

Rollback: remove the observer inventory and restore prior images. Retain the
additive table and all existing recovery records. Never delete liabilities or
rebroadcast operations as part of a readiness rollback.

## Diagnostics and local verification

Stable events `worker_funding_observation` and `worker_funding_monitor_failed`
report role, state and reason. Changed states emit immediately; repetitions are
summarized every 60 seconds. The separate PostHog projection drops balances,
liabilities, amounts, addresses, hashes, raw transactions and provider messages.
CloudWatch's original alarm fields and records are preserved. Frontend incidents
use the existing categorical allowlists and outage deduplication.

Verification includes backend policy/encoding tests, fixed-block RPC/reorg/lens
outage tests, funding nonce/replacement/concurrency tests, real PostgreSQL
migration/recovery-byte tests, native-AA integration, frontend parser and action
isolation tests, Terraform mocked canary plans, and actual Fluent Bit dual-output
privacy fixtures. Live Frankfurt activation, operator reserve/inventory review,
PostHog ingestion and the wider release/performance gates remain separate.

Local results (2026-09-11): 1,101 backend unit tests, 12 native-AA integration
tests, 37 worker/PostgreSQL tests, 1,341 frontend unit tests, 32 mocked Frankfurt
plans and both real Fluent Bit routing cases passed. TypeScript, targeted ESLint,
Lua projection fixtures and whitespace checks passed. No live performance or
on-chain acceptance result is claimed for these changes.
