# Oracle feed synchronization and automatic review recovery

Status: worker and frontend recovery implemented; atomic contract synchronization proposed for a separate release.

## Problem and current release

Historical execution parses the unique first post-commit Pyth tick and may advance the engine's neutral mark. Pyth's `parsePriceFeedUpdatesUnique` does not update stored feeds. A subsequent live basket read can therefore have an oldest component timestamp below `engine.lastMarkTime()` and revert with `PletherOracle__PriceOutOfOrder` even though execution succeeded.

The worker now checks all six configured Pyth feeds and the engine mark at one block. Five-second health checks detect lag independently of the normal 30-second refresh cadence. When the oldest cached component covers the mark, the worker simulates and submits the existing `updateMarkPrice` call, including equal-timestamp repair. It never considers a newer maximum component timestamp sufficient. Its receipt hash survives RPC errors/timeouts within the running process, preventing a second submission until the first resolves. A process restart does not preserve that in-memory hash; check the updater's pending transactions before restarting a stalled worker.

Review retries only decoded oracle/router ordering failures, two seconds after each failed attempt, within the original 30-second preparation deadline. Every retry rebuilds the full review at a fresh block. Inputs and previously displayed terms remain visible, but only a successfully rebuilt review can enable confirmation. No user transaction is retried or submitted automatically. Recovery telemetry contains outcome and duration, not account identifiers or raw errors.

Once prepared, the visible review keeps its quoted terms across market/account polling and tab visibility changes. At 45 seconds before its deadline, confirmation is disabled and the user must explicitly retry the review for fresh terms. Editing order inputs or changing identity still invalidates the prepared order; current account validation and submission checks remain in force.

The current release does not weaken oracle guards, change historical execution pricing, persist browser drafts, or change a public API or database schema.

## Deployment and acceptance

1. Run worker Node tests, review/controller/error tests, frontend unit tests, lint, production build, and Terraform formatting/validation.
2. Verify deployed oracle/router binding and reproduce separate historical/stored Pyth state on a local deployment or fork. Confirm equal-timestamp `updateMarkPrice` updates storage and permits a subsequent live read without changing the historical fill price.
3. Deploy backend first, then frontend to Sepolia using GitHub CLI. Follow AGENTS.md authentication, immutable SHA, duplicate-run, monitoring, and public smoke-check requirements. `bootstrap` must remain false.
4. Terraform exposes `perps_oracle_updater_health_poll_seconds` (5); the worker uses the same default if an existing ECS task definition has not yet acquired the environment variable. `perps_oracle_updater_poll_seconds` remains 30. Apply reviewed Terraform through the normal infrastructure workflow when updating task definitions; no broad infrastructure apply is necessary just to activate the default.
5. Confirm worker startup cadence, stored component timestamps versus mark, and recovery after execution. A UI test must keep the draft intact and require explicit confirmation after successful recovery. Do not create a real user trade merely to test rollout.
6. Monitor `oracle_sync_lag`, `oracle_update_mined` (including `repair`, `synchronized`, fee and lag), `oracle_sync_pending`, and `oracle_worker_iteration_failed`. In frontend analytics compare `perps oracle recovery` started/succeeded/exhausted/failed events and duration. Investigate recurring lag or exhaustion; healthy operation should not produce repair writes on every health tick.
7. Roll back frontend and backend independently to known-good image/build revisions if failures or excess transactions appear. The worker uses existing contracts and performs no migration.

Do not claim deployed verification from local source inspection or mocked tests. Record actual rollout SHA, workflow results, and smoke evidence separately.

## Verification evidence (2026-09-15)

- `npm run test:oracle-sync-fork` passed against Arbitrum Sepolia block 309107473 using the deployed oracle/router/engine. It reproduced the ordering error after historical resolution and authorized mark installation, repaired all six feeds at the same timestamp, and checked that the historical execution price remained unchanged.
- The fork test substitutes Pyth signature verification/storage with independent historical and stored state. It does not broadcast or claim to test Pyth cryptography, a complete user trade, or a live production incident.
- `oracleSyncRecovery.test.tsx` exercises the actual worker iteration and review controller together with injected chain I/O; it checks preserved draft inputs, a fresh block, and no user submission.

## Future phase: synchronize atomically in the contracts

This is a follow-up design, not part of the current implementation. Its desired invariant is: an execution that installs a mark leaves every required stored Pyth feed at least as recent as that mark in the same transaction.

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
