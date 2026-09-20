# Liquidation keeper recovery

This patch targets the existing v1.2.3 contracts. It does not deploy contracts,
change liquidation eligibility, or relax global backend price admission.

## Evidence and remaining uncertainty

On September 18, 2026, FAD raised maintenance requirements before Friday's close.
Only 30 of 211 initially exposed positions were liquidated before close. The
second transaction attempted 17/20 accounts at 8,066,462 gas; a read-only replay
at the same historical state attempted all 20 with 15M gas. The backend execution
payload gate used a maximum age of ten seconds even in the three-day frozen
regime. Saturday simulations demonstrated that the deployed contracts still
accepted the Friday payload.

Deployed-image/configuration and runtime-log verification remain necessary:
AWS authentication had expired during the investigation. Do not describe the
source-level confirmation-delay path or payload gate as a confirmed runtime
incident cause until those logs are checked.

## Behavior

- Resolve frozen policy, age limits, timestamp and block gas limit before each
  batch. Live mode keeps the stricter backend freshness rule; frozen mode uses
  the deployed contract's limit. Cache source/metadata checks remain in force.
- Snapshot candidate membership once per sweep in least-recently-checked order.
  Process all pages without an idle delay between pages. This snapshot is kept
  in memory; the original database order resumes fairness after restart.
- Read up to 100 account snapshots per multicall (previously ten). The candidate
  page remains 1,000. Database updates and transaction execution remain serial.
- Verify the exact transaction gas envelope at a fixed block and require a
  complete attempted prefix. Increase gas to a configured ceiling, then split.
  The ceiling defaults to 25M and is also capped at 80% of block gas limit.
- Wait for confirmation depth at one-second cadence, re-read the receipt, and
  continue immediately. Keep the persisted nonce unresolved on reconciliation
  failure. Suffix retries refresh policy and payload and are bounded to three.
- Preserve existing signing, affordability, nonce replacement, and payload
  circuit-breaker behavior. Policy changes invalidate old payload suppression.

## Validation performed

Base commit: `ce1806f000277d9b61f7e9a4bd3032a891d290f9`.

- Backend unit suite: 1,248 examples, zero failures, including weekend admission,
  reopening rejection, historical gas-envelope regression, partial retries,
  confirmation waiting, and an 8,000-account/250-risk scheduler fixture.
- Read-only public-RPC benchmark: 7,618 account-lens reads, 77 multicalls,
  21.88 seconds at Arbitrum Sepolia block 310645101; maximum request 0.43s.
  The proportional 8,000-account read estimate is 22.98s. This excludes database
  updates, classification, simulations, confirmations, and settlements; it is
  not an end-to-end liquidation throughput measurement.

- Actual Haskell gas-sizing smoke test at block 310646489: a 20-account
  batch required successive 5M, 10M, then 20M envelopes to complete all attempts.
  This used read-only `eth_call`; no transactions were sent.
- Liquidation-worker executable builds; Terraform 1.16.0 validation passes.

Reproduce reads with:

```sh
python3 scripts/benchmark-liquidation-reads.py --snapshot /path/to/snapshot.json
```

The snapshot contains `addresses.cfdEngineAccountLens` and
`positions: [{"account": "0x..."}]`. The script neither signs nor broadcasts.

## Deployment gates

1. Follow repository AWS/GitHub authentication guidance. Capture active task
   definition, image digest, configuration, pending signer nonce and recent logs.
   Confirm the app's engine/router addresses match the v1.2.3 manifest.
2. Run unit tests and a dry run using the production database/RPC configuration.
   Override an existing `LIQUIDATION_WORKER_POLL_SECONDS=600` explicitly; changing
   a default alone does not override Terraform inputs. Set poll=5,
   multicall=100, execution batch=20 and maximum gas=25000000.
3. Confirm frozen-mode payload admission and the gas ceiling with the actual
   provider. Require complete scan reads/classification within 60 seconds in
   the measured environment. Use a fork/staging load of 8,000 accounts and 250
   risks to verify settlement throughput within five minutes under healthy
   chain/RPC conditions. Do not claim either gate from the unit fixture alone.
4. If the read/classification gate fails, identify RPC versus database cost
   before adding concurrency. The current patch intentionally keeps a single
   execution owner; a concurrent discovery queue is not included.
5. Deploy exactly one worker. Preserve pending rows and reconcile the outstanding
   nonce first. Inspect one confirmed batch's per-account outcomes before
   declaring backlog recovery, then reconcile the full newly discovered set.
6. Pause new submissions if receipt invariants or signer nonce ownership fail.
   Preserve pending rows during rollback; do not start a second signer or delete
   raw signed transactions. A rollback restores the old weekend limitation and
   must not be presented as restored service.

## Monitoring

The patch adds one-minute operations alarms for slow sweeps, blocked payloads,
and receipt invariant failures. They use the existing SNS alarm destination.
A slow-sweep duration includes transaction execution; distinguish slow discovery
from a sweep legitimately spending time settling a backlog.

Check `liquidation_sweep_finished.completed`, duration, candidate count,
`liquidation_execution_policy`, and confirmed receipt outcomes. An incomplete
sweep, successful transaction, or completed simulation is not a cleared backlog.
Recheck all remaining candidates at current policy/price and explain each as
liquidated, closed, solvent, or explicitly blocked.

## Durable backlog monitoring and end-to-end follow-up

The additive schema migration introduces per-candidate first-risk/check times and
per-engine confirmed-progress time/block. Startup runs it idempotently. Existing
rows begin with unknown observation age; they are not silently classified healthy.
Schema rollback is unnecessary: the old worker ignores the added columns.

The watchdog samples every ten seconds on a separate database lease and runs only
inside the active worker's advisory lock. Dry runs do not emit health samples, and simulated actions never count as
confirmed progress. Existing pending receipts still use canonical reconciliation. Risk is the conservative keeper candidate queue,
including its preflight buffer; receipt-based solvency clears that queue entry.
Only fully reconciled receipts with matching engine liquidation events advance
progress. Re-reading the same or older block does not advance it again.

New alarms use the existing operations SNS destination:

| Signal | Threshold |
|---|---|
| Oldest unresolved risk | 300 seconds |
| Backlog with no new confirmed liquidation | 60 seconds |
| Oldest successful risk classification / never-checked candidate | 60 seconds |
| Missing watchdog samples | Two consecutive one-minute periods |

A heartbeat proves the watchdog can read the database; it does not prove the
executor is progressing. Age/progress alarms cover an executor blocked on RPC.
Missing samples are a separate alarm, never interpreted as an empty backlog.

Validation on September 19:

- Eight real PostgreSQL tests pass: first-observation persistence, unknown reads,
  restart, resolved/recurrent risk, pending versus confirmed work, replay-safe
  progress, transaction rollback and engine/chain isolation.
- Full backend unit suite: 1,248 passed. All 28 mocked Terraform tests passed,
  including age thresholds, metric fields, heartbeat loss and SNS routing.
- The actual worker executed **40 liquidations in two mined transactions** on an
  isolated Arbitrum Sepolia fork, with one-block confirmations and real
  PostgreSQL writes. Runtime was **4.728118 seconds with a warmed fork cache**.
  All 40 positions closed; pending rows and the monitored backlog cleared.
- Fork receipt blocks: 310651464 and 310651466. These are **local-fork receipts**,
  not public Arbitrum Sepolia transactions.
- The first cold-fork attempt failed account reads on archival-RPC timeouts. The
  watchdog continued emitting increasing backlog age during the stall. Warming
  the two contract-call batches took 41.08s and 28.19s before the successful run.
  Do not interpret the warmed execution time as a production latency guarantee.

The fork test uses `scripts/liquidation-fork-smoke.hs` and the public signed fixture
`scripts/fixtures/liquidation-fad-v123.json`. It refuses non-Anvil RPCs and uses only
hard-coded loopback endpoints: PostgreSQL port 58439, database
`keeper_fork_critical_path`, and Anvil port 58546. Start Anvil from the v1.2.3
Arbitrum Sepolia state before these positions were liquidated, with chain ID
421614 and one-second block mining. Warm the fixture batches using eth_call if
the archival provider is slow, then run from `apps/backend`:

```sh
cabal exec --offline -- runghc -package=plether-api ../../scripts/liquidation-fork-smoke.hs ../../scripts/fixtures/liquidation-fad-v123.json
```

The test truncates only its explicitly named disposable local database. Reset the
fork before repeating: successful test transactions have already closed positions.

**Production E2E gate passed on September 20:** after explicit user approval,
the patch ran against the production database and public Arbitrum Sepolia,
liquidated 156 positions and verified all receipts and database reconciliation.
See the measured results below. Permanent deployment remains separate.

### September 20 production preflight

AWS profile `plether` authenticated successfully. The live service remains on
`plether-sepolia-liquidation-worker:61`, image revision
`ce1806f000277d9b61f7e9a4bd3032a891d290f9`, with one running worker, poll 600
seconds and multicall size 10. Deployed logs directly confirm repeated
`Executable Pyth payload was outside the latest freshness window` failures.

A short-lived read-only ECS query against the production `plether` database at
06:35:29 UTC found 7,558 candidate rows, no pending transactions, and 215 rows
with that stored freshness error. This is an error count, not an eligibility
count. The signer had matching latest/pending nonce 679 and 0.904163562650262
testnet ETH. Contract reads confirmed frozen mode and the 259,200-second
frozen payload limit; the latest stored payload was within that limit.

An additional public-chain `eth_call` at block 310790031 simulated 20 selected
production candidates to completion (`nextIndex = 20`) with 10,000,000 gas.
Fifteen were liquidatable under the stored-mark lens check. Simulation duration
0.167 seconds does not measure transaction inclusion or reconciliation latency.

The patched Linux ARM64 image built successfully and was uploaded for a
one-off test, pinned at:

`932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-api-sepolia@sha256:845d792432821a58d1c1d9b146b388545d413656daa592fa26bb69f36b044cfe`

### Approved live execution and independent verification

The user explicitly approved pausing the old keeper, a patched dry run followed
by a full live sweep, and restoration of the original service. This resolved
the automatic review's earlier scope block. The controller ran both tasks to
exit 0 with the existing exclusive database advisory lock.

- Dry run: **7,839 registry rows in 38.254975637 seconds**, no execution errors.
  Registry rows include already-closed positions re-seeded from history.
- Live sweep: **70.728526012 seconds**, including confirmed reconciliation.
- **12 successful transactions; 156 liquidated accounts; 59 solvent skips;
  no failed batch items or partial batches.**
- Public receipt span: blocks **310800174–310800419**, 07:32:10–07:33:12 UTC
  (**62 seconds**). Submission-to-reconciled confirmation took **0.499–1.595
  seconds**, median **1.570 seconds**.
- Independent RPC verification matched canonical receipt hashes, router batch
  outcomes and engine liquidation events. All 156 liquidated positions were
  closed at block **310800646**. Signer latest/pending nonce both **691**.
- Separate read-only production SQL at **07:34:13 UTC** found **7,400 candidate
  rows**, **0 pending**, **0 unresolved risks**, **0 never-classified rows**,
  and **0 rows with errors**. Confirmed progress persisted at block **310800419**.
- Nine watchdog samples were emitted; final monitored risk and pending counts
  were zero. Maximum sampled unresolved-risk age was **5.95 seconds**.
- The live pass emitted the expected >60-second slow-sweep warning; final
  oldest-classification age was **67.25 seconds**. This was a progressing
  executor, not a stalled one. Do not promise an entire future FAD backlog
  clears within a few seconds from this result.

Dry task: `0f89c89165334eb3b65e947ac2219b67`.
Live task: `e8e5262b22254943bdd271c4fce7c2f1`.
Independent SQL task: `97bdfac0c8e241e7aaf963e04fcbe493`.
First transaction: `0x8c247deb8f4f0d1556df3203e3c06bca1efa5592e99f1e0392d70ee5f1532089`.
Last transaction: `0xed277cd4135a610a4e1507e024fda5849c62db39172abe22610b352b73ff1b37`.

**Restoration verified:** original task definition revision 61 has desired=1,
running=1, pending=0. This one-off test did not permanently deploy the patch or
CloudWatch alarms. The additive monitoring schema/progress and real testnet
liquidations persist. The restored old service still has its earlier freshness
limitation; permanent rollout must follow the normal repository workflow.
