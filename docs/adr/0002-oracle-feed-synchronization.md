# Oracle feed synchronization and automatic review recovery

Status: worker health corrected for Core v2's accepted basket; local validation does not imply deployment. The original stored-feed design is retained below as historical context.

## Problem and current release

Core v2 (`config/perps/arbitrum-sepolia-v2.0.0.json`) keeps an accepted live basket independently of Pyth's mutable stored feeds. A normal Router refresh parses a signed basket and records it in the Oracle without updating Pyth storage. Comparing the engine mark to Pyth's oldest stored component therefore reports false lag and can bypass the normal refresh interval indefinitely.

The worker reads `getAcceptedBasketMark()`, protocol status and `getLatestPrice(MarkRefresh)` at one block. Core's mode-aware view validates every accepted component's freshness, divergence and confidence, plus basket ordering and equal-time price consistency. `lag_seconds` now measures the accepted basket's newest timestamp against the engine mark, not Pyth storage; a zero ordering lag alone does not prove health. Known decoded basket-validation failures trigger early recovery. Transport, decoding and unknown errors propagate without authorizing a paid refresh. Frozen policy validates the engine mark and does not submit ineffective live-basket updates.

Five-second health checks detect a real failure independently of the normal 30-second refresh cadence. Payload ordering uses the newest component, matching Core v2; the oldest component still must meet the local age bound, and simulation enforces all contract guards, including individual accepted-feed monotonicity. Equal-timestamp payloads are allowed only for recovery. No guard or pricing rule is relaxed. Receipt hashes survive RPC errors/timeouts within the running process, preventing a second submission until the first resolves. A process restart does not preserve that in-memory hash; check the updater's pending transactions before restarting a stalled worker.

Historical execution can still advance the engine mark beyond the accepted live basket. A subsequent view may then revert with `PletherOracle__PriceOutOfOrder` until a Router refresh advances the accepted basket. That genuine condition remains a recovery trigger even when Pyth storage is already current.

Review retries only decoded oracle/router ordering failures, two seconds after each failed attempt, within the original 30-second preparation deadline. Every retry rebuilds the full review at a fresh block. Inputs and previously displayed terms remain visible, but only a successfully rebuilt review can enable confirmation. No user transaction is retried or submitted automatically. Recovery telemetry contains outcome and duration, not account identifiers or raw errors.

Once prepared, the visible review keeps its quoted terms across market/account polling and tab visibility changes. The provisional simulation deadline does not expire the preview. Confirm assigns the new request a deadline from the latest block timestamp plus the router’s current `maxOrderAge`, preserving all reviewed economic terms, then simulates against current state before signing. Once assigned, the request is immutable across retries; recorded attempts must use activity recovery. Editing order inputs or changing identity still invalidates the prepared order; current account validation and submission checks remain in force.

The current release does not weaken oracle guards, change historical execution pricing, persist browser drafts, or change a public API or database schema.

## Deployment and acceptance

1. Run worker Node tests, review/controller/error tests, frontend unit tests, lint and production build. This worker correction changes no Terraform configuration.
2. Run `npm run test:oracle-sync-fork` with `ARB_SEPOLIA_RPC_URL`. The test verifies the canonical v2 Oracle/Router runtime hashes on a local fork, reproduces a healthy accepted basket alongside old Pyth storage, and recovers an actual accepted-basket ordering failure. Pyth verification is substituted; no transaction is broadcast and this is not a cryptographic or complete trade test.
3. Deploy the backend to Sepolia using GitHub CLI. Follow AGENTS.md authentication, immutable SHA, duplicate-run, monitoring, and public smoke-check requirements. `bootstrap` must remain false. This correction needs no frontend deployment or contract change.
4. Terraform exposes `perps_oracle_updater_health_poll_seconds` (5); the worker uses the same default if an existing ECS task definition has not yet acquired the environment variable. `perps_oracle_updater_poll_seconds` remains 30. Apply reviewed Terraform through the normal infrastructure workflow when updating task definitions; no broad infrastructure apply is necessary just to activate the default.
5. Confirm worker startup cadence, accepted-basket validity versus the engine mark, and recovery after execution. A UI test must keep the draft intact and require explicit confirmation after successful recovery. Do not create a real user trade merely to test rollout.
6. Monitor `oracle_sync_lag`, `oracle_update_mined` (including `repair`, `synchronized`, fee and lag), `oracle_sync_pending`, and `oracle_worker_iteration_failed`. In frontend analytics compare `perps oracle recovery` started/succeeded/exhausted/failed events and duration. Investigate recurring lag or exhaustion; healthy operation should not produce repair writes on every health tick.
7. Roll back frontend and backend independently to known-good image/build revisions if failures or excess transactions appear. The worker uses existing contracts and performs no migration.

Do not claim deployed verification from local source inspection or mocked tests. Record actual rollout SHA, workflow results, and smoke evidence separately.

## Core v2 correction validation (2026-11-05)

- Worker regressions cover a healthy accepted basket with obsolete Pyth storage, normal refresh cadence, real ordering/staleness/missing-basket/equal-time failures, frozen-policy transitions, RPC failures, retained receipts and reorg-aware reads.
- The current v2 fork test passed at Arbitrum Sepolia block 326350507 with Oracle and Router runtime hashes checked against the release manifest. Normal refresh left Pyth storage old while the accepted basket became valid; an authorized engine mark advancement then reproduced a real ordering failure and a subsequent refresh repaired it. Pyth verification was mocked, not broadcast.
- A read-only run of the actual worker against public Sepolia accepted the current basket and returned `oracle_update_not_needed` with zero ordering lag. This was dry-run validation, not a backend rollout or proof of future transaction success.

## Historical verification evidence (2026-09-15, previous deployment)

- `npm run test:oracle-sync-fork` passed against Arbitrum Sepolia block 309107473 using the deployed oracle/router/engine. It reproduced the ordering error after historical resolution and authorized mark installation, repaired all six feeds at the same timestamp, and checked that the historical execution price remained unchanged.
- The fork test substitutes Pyth signature verification/storage with independent historical and stored state. It does not broadcast or claim to test Pyth cryptography, a complete user trade, or a live production incident.
- `oracleSyncRecovery.test.tsx` exercises the actual worker iteration and review controller together with injected chain I/O; it checks preserved draft inputs, a fresh block, and no user submission.

## Historical proposal: synchronize atomically in the contracts

The following records the original follow-up design. Core v2 already synchronizes Pyth storage for successful historical resolution and quotes a separate execution fee. Its accepted live basket is a separate state source. These historical design notes must not be used to restore the obsolete stored-feed worker check or to claim the accepted basket always advances with historical execution.

### Execution semantics

After a successful unique historical parse, update Pyth storage from the same signed payload before allowing the engine mark to advance. Continue using the parsed historical tick for execution, the neutral basket for the mark, and the existing adverse-confidence adjustment for the fill. Never substitute a newer live price for the order's unique settlement tick. Preserve settlement windows, confidence/divergence checks, cap handling, and mark monotonicity.

If feed synchronization fails, the execution must not install a newer mark. Define transaction and per-item rollback boundaries explicitly: a whole-transaction revert restores Pyth and engine state; a later caught order-policy failure may retain an already synchronized neutral mark according to the existing router lifecycle policy. Synchronization must not allow a failed item to corrupt cache or aggregate fee accounting.

Frozen-oracle execution already follows the stored-feed update path; avoid duplicate updates/fees there. Already-newer Pyth prices must never move backwards. Historical fills older than the current mark must retain their historical semantics and must not roll the mark back.

### Fees, batches, and compatibility

A fresh historical parse plus a stored-feed update requires payment for both Pyth operations. Introduce an execution-specific fee quote rather than silently changing the generic single-update quote used by mark refresh/liquidation. Update router funding checks, keeper single/batch quotes, total-spent bookkeeping, snapshot fee reporting, refunds/deferred refunds, and insufficient-fee handling together. Distinguish fees consumed from fees returned on an unavailable historical tick.

Synchronize once for each newly parsed basket; subsequent orders reusing the same proven, synchronized batch basket pay no additional Pyth fee. Validate which payloads a cache entry covers. Do not reuse a cache after a failed synchronization or assume an externally supplied cache proves synchronization.

Before implementation, map upgrade/configuration paths for the deployed oracle, immutable router sidecars, and release manifests. Specify whether replacement contracts are required, how pending orders are drained/handled, how configuration hashes invalidate reviews, and how keeper versions are gated against the matching contract ABI. Release the affected contracts and keeper fee logic together; there is no claim that the existing deployment can be upgraded in place.

### Required validation before the future release

- A realistic Pyth mock with independent historical payloads and stored feed state, plus a fork test against the actual supported Pyth deployment.
- Successful execution immediately followed by live price/review reads; unchanged neutral mark and adverse fill prices.
- Equal timestamps, newer stored prices, older historical execution, missing/divergent feeds, frozen/FAD modes, and unavailable unique history.
- Single execution, mixed batch/cache reuse, failed items, synchronization reverts, insufficient fees, exact fees, excess/refunded fees, and rollback boundaries.
- Gas measurements for new parses versus reused baskets; keeper balance/bounty economics and configured transaction gas limits.
- A coordinated compatibility, migration, deployment, monitoring, and rollback plan reviewed before implementation.
