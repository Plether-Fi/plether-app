# Latched SL/TP integration (core PR #78)

This app targets core commit `d3b28d1520d46a730e3179303b70761e315e0ce2`'s
protection lifecycle. Request and intent hashing stay V2; lifecycle receipt and
execution-config domains are V3. Earlier contract generations are rejected.

## Release gate

Deploy a fresh complete perps stack, then replace the pinned release artifact,
frontend addresses, AA manifest, backend and keeper configuration, runtime code
hashes and indexer start block together. The addresses currently checked into
this repository describe the previous deployment; do not deploy this app update
against that stack. Verify both `CONFIG_SCHEMA_HASH` and `RECEIPT_TYPEHASH`,
`currentExecutionConfigHash`, immutable component bindings and the AA deployment
manifest before enabling traffic. Enable protection commits through the existing
48-hour timelocked configuration process. No in-place state migration is supported.

## Keeper behavior

The order keeper owns the retry job under its existing advisory lock and signer.
It discovers the protection Book through the Router and verifies the V3 schemas
and Book/Router/Engine bindings at startup. Normal FIFO execution and expiry
cleanup run first. Failed protection attempts retain their bounty and cleanup
may earn nothing; fund the keeper for this gas cost.

The retry job automatically submits only when the latest immutable outcome is
`Expired`, failed, has `RetainedForProtectionRetry` (4), a zero recipient and the
exact bounty now retained by the active `Latched` (8) protection. It checks exact
position side/size and no pending account orders, then simulates the nonpayable
`retryPositionProtectionClose(uint64)` call on the Book. Retry does not accept
Pyth data, re-evaluate the trigger, or reserve another bounty.

Admission conservatively waits for an **empty on-chain FIFO**; no queue-drain
throughput is assumed. This is stricter than the core's permitted projected
head-arrival policy. The configured active poll delay must fit within
`maxOrderAge - 15 seconds` (45 seconds at the default TTL). A fresh six-feed cached
payload must satisfy the current close execution staleness/divergence policy,
including the frozen-close path. Pausing, disabled protection commits or engine
degradation do not independently disable retries.

Each cycle submits at most one retry and rotates through at most 16 candidates.
A 60-second persisted submission backoff bounds retry races and uncertain RPC
results. Confirmed child order events are ingested immediately so the ordinary
keeper can execute the new attempt without waiting for its log cursor.
Deterministic failures stay latched and emit `protection_retry_held_*` warnings
with reason, failure fingerprint, attempt count, queue tail, projected arrival
and latched age. Remediate the cause before making a manual permissionless retry.

## Permanent evidence and frontend

`perps_protection_attempt_events` retains queued, failed and lifecycle registration
events with transaction/log/block provenance. Every queued row records both the
new and previous order ID. Registration remains after `isProtectionAttempt`
becomes false on finalization. Join by Book and order ID; never overwrite the
history with the latest `linkedOrderId`. The journal rebuilds from the release
start block if its confirmed checkpoint is reorganized. Candidate state is a
scheduling cache; on-chain state is always authoritative.

Order receipts/history include bounty disposition and recipient, so retained
reserves are distinguishable from keeper payouts. The frontend keeps discretionary
orders locked in `Triggered` and `Latched`, explains the retained bounty, and
allows a connected wallet to retry a latched close while paying network gas.
The action queues a market-close attempt; execution timing and price are not
guaranteed. A triggered or latched protection cannot be cancelled.

Before release, rehearse trigger → expiry → unpaid cleanup → latch → fresh retry
→ execution on the newly deployed testnet stack, along with deterministic failure,
liquidation, unavailable oracle, paused/degraded/frozen modes and restart/race cases.
Verify the one-time bounty payment and complete attempt history.
