# Order-expiry reliability release

## Phase 1: existing V2 contracts

The V2 absolute deadline covers confirmation, wallet approval, inclusion,
queueing and execution. Preview preparation uses a provisional deadline; time
spent reading a completed review does not consume the final execution window.

- Keep prepared review terms stable across polling and tab visibility changes.
  Confirm assigns a new, unrecorded request its deadline using a fresh block and
  the router's `maxOrderAge` at that block, without changing any economic bounds.
  Simulate the finalized request against current state before sponsorship/signing.
  Never reassign the deadline after finalization or for a recorded attempt;
  activity recovery retains existing signed/submitted requests.
- Server HTTP Date (including precision, Age and transport margin), advanced
  with a monotonic timer, replaces the device clock for signing checks. Refresh
  timing before signing and after wallet approval, including after device sleep.
- Require 45 seconds before opening the signing prompt and 30 before submitting,
  using the earlier order/sponsorship expiry. The gateway independently applies
  the 30-second guard immediately before relay. Missing timing fails closed.
- A late signed operation stays in recovery. A repeated gateway submission may
  already have reached the network and is reported as uncertain, not unsent.
  Only authoritative recovery can retire the saved liability.
- Historical reveal fetching has two independent workers, separate from latest
  updates and chart backfills. Exact reveal ticks are deduplicated, in FIFO
  priority order, with a three-second HTTP response timeout and ten-second job
  timeout. Provider 429 backoff is shared with latest-price requests, including
  Retry-After dates. A slow latest update does not hold the reveal worker.
- Keeper candidate reads include the actual FIFO head even after a recent
  attempt. Independent reads are concurrent at one block; batch candidates stay
  contiguous. Confirmed progress schedules another iteration immediately. Receipt
  polling is 250ms for the first five seconds, then one second, with a monotonic
  two-minute timeout and durable reconciliation afterward.
- The keeper saves signed transaction bytes before broadcasting. An unresolved
  journal blocks new execution transactions across process restarts. Retry uses
  the same signed bytes/hash, and application of a receipt and journal clearance
  is atomic. No replacement or fee bump is invented for an uncertain broadcast.

HTTP timing is only a conservative client freshness check. Contracts and the
backend remain authoritative for authorization, financial bounds and retirement.
The existing exact historical-price and execution-mode policies remain enforced.

## Sepolia release procedure

1. Review and merge the Phase 1 application PR. Deploy the backend (including the
   basket worker, keeper and log-router) before the frontend, using the `gh`
   workflow procedure in AGENTS.md. Verify both workflow SHAs and endpoint health.
2. Verify uncached readiness/preparation responses expose Date. Test reviews with
   clocks a day ahead/behind, tab suspension, and 0/15/30/60/119/121-second wallet
   delays. V2 approvals that cannot retain 30 seconds must remain unsent and
   recoverable; those rejections are not counted as successful closes.
3. Exercise twenty-order bursts with a slow head, missing historical payloads,
   provider throttling, RPC failures and keeper restarts. Verify FIFO, partial
   batch progress, exact price validation, and no duplicate order/nonce after an
   unknown broadcast. Keep oracle and chain outage cohorts separate.
4. Measure preparation, wallet wait, inclusion, historical-price availability,
   queue wait and execution independently. Under representative healthy peak
   load require inclusion-to-terminal p99 below 20 seconds and zero avoidable
   expirations in the controlled cohort. Report successful closes, submission
   rejections, unresolved recovery and expirations together.
5. Verify the canonical monitoring from `order-reliability.md`; activate its
   prepared alerts only after telemetry and the required PostHog scopes exist.

Use `scripts/operations/order-expiry-gates.sql` for canonical commitment counts,
successful closes and latency percentiles alongside browser/backend attempt stages.
The indexer now records `isClose` on IntentRegistered events; older unclassified
rows remain explicit instead of being counted as opens or successful closes.
Historical fetching emits `reveal_payload_fetch_completed` with request duration
and `commit_to_price_available_ms`; this latter measurement includes the wait for
price availability and must not be added to commitment-to-terminal latency.

These performance gates have not been satisfied by local unit tests. Record
workload, deployment SHA, interval, eligible orders, successful closes, rejections,
recovery waits, expirations and latency percentiles before promoting the release.

## Rollback and recovery

Frontend rollback does not remove the backend guard. Database changes are
additive; keep the broadcast journal on rollback. Before downgrading a keeper to
code that predates the journal, stop it and verify this read-only query returns
zero for the deployment:

```sql
SELECT count(*) FROM perps_keeper_broadcasts WHERE order_router = :router;
```

If it does not, keep the new reconciler running or pause execution for operator
investigation. Do not delete a journal to unblock the queue and do not run an old
keeper alongside it. Saved signed bytes are liabilities, not failed requests.
An RPC/chain outage may keep the queue blocked; expose the diagnostic rather
than submit another transaction whose predecessor is unresolved.

## Phase 2: protocol release boundary

The separate contract candidate introduces `commitOrderV3`: signed `submitBy`
(default fresh review +120 seconds) and `executionWindowSeconds` (default 60).
Commitment resolves the latter to an immutable execution deadline. V2 entrypoints
retain their original absolute deadline semantics; replay never restarts a timer.

**Additional migration constraint found during implementation:** CfdEngine's
`setOrderRouter` is one-shot, and the lifecycle/protection graph is immutable.
A V3 router alone cannot replace the router servicing existing positions. The
migration must explicitly choose a separate V3 market with V2 service retained,
or a separately designed/reviewed position migration. No production addresses,
manifest pins or user positions are changed by Phase 1.

V3 remains inactive until the migration choice, versioned application,
sponsorship and recovery routing, contract review and invariant tests are complete.
Before activation rehearse the migration and observe at least 1,000 eligible
Sepolia orders over at least 24 hours, with platform-caused expirations below 1%.
Keep all V2 services and deployment-specific recovery available while old positions,
orders or signed authorizations remain. Never reinterpret a V2 signature as V3.
