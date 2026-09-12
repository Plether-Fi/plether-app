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

## Singapore activation (separate approval required)

Follow [the release procedure](singapore-sepolia-aa-release.md). Apply the additive
funding migration before starting the observer. API roles read the observation
table; the observer reads existing recovery journals and may SELECT/INSERT/DELETE
its observation rows. It receives no signing keys or spending authority.

Set aa_funding_monitors to the reviewed complete six-role public signer inventory.
Use actual configured maximum batch gas, value and fee buffers; do not invent
funding estimates. Set aa_funding_monitor_image to the tested backend digest.
Terraform permits only the consolidated Singapore Sepolia native-AA deployment.
The backend deployment workflow promotes this container with the other application
containers; verify its digest, inventory hash and absence of private-key secrets.

Local tests are not proof of live reserve accuracy, provider pending-body coverage,
healthy idle readiness or PostHog ingestion. Those are release gates.
