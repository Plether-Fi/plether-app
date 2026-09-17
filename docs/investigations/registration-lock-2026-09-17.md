# Registration session lock investigation — 2026-09-17

## Finding

The Insights snapshot worker holds a PostgreSQL `FOR UPDATE` lock on the competition row while recomputing funding-integrity flags for every participant. The recalculation takes roughly 140 seconds in the observed publication transaction. Registration session creation waits for that competition row during its foreign-key check. The frontend awaits the request without a timeout and displays `Starting…` throughout.

This is recurring contention caused by transaction scope and an expensive query. The observed registrations had already completed server-side Turnstile verification. The API connection pool was not exhausted.

The inspected `Plether.Database.Insights` source matches the deployed revision `9cbd2dd2af2ef8a83a0cd3837d0479351775f86b` byte for byte.

## Incident evidence

All times below are UTC; add two hours for Warsaw.

- At 15:51:01, registration connection PID 15461 was waiting on a `transactionid` lock held by PID 13650. The blocker transaction was already 58.8 seconds old.
- At 15:51:31, a second registration connection, PID 15460, was waiting on the same blocker.
- At 15:52:22, the blocker transaction was 139.9 seconds old; registration waits were approximately 84.5 and 55.3 seconds.
- At 15:52:26, the two session POSTs returned HTTP 200 after 89.143 and 59.359 seconds respectively. Database connection hold times account for nearly all of that latency.
- At 15:52:27, both connections were released and the blocking snapshot was empty.
- API pool observations during the incident show zero queued checkouts. Requests already owned connections and were waiting in PostgreSQL.

A subsequent read-only inspection of PID 13650 confirmed:

- Its client address, `10.0.2.81`, matches the running Sepolia worker ECS task.
- Its active statement is `fundingIntegrityRefreshSql` for `testnet-trading-2026-09`.
- It retains write locks on snapshot tables and a row-share table lock on `insights_competitions`, consistent with snapshot publication inside the transaction that first took the competition row lock.
- At capture, the statement had run for 101.388 seconds and its transaction for 101.892 seconds. This shows the integrity statement accounts for almost all of that transaction's elapsed time.
- Other diagnostic samples show the same connection repeatedly running transactions longer than a minute.

Evidence sources: CloudWatch `/ecs/plether-sepolia` events `db_pool_observation`, `db_blocking_snapshot`, and `api_foreground_request_completed`; read-only `pg_stat_activity`, `pg_locks`, table/index metadata, and a non-executing query plan.

## Code path

1. `Plether.Insights.SnapshotWorker.updateCompetition` calls `listCompetitionParticipants` before collecting account snapshots.
2. `listCompetitionParticipants` has a write side effect: it calls `refreshCompetitionIntegrityFlags` before selecting the participant list (`Database/Insights.hs:1818`).
3. After external account reads, `publishAccountSnapshotBatch` opens a transaction (`Database/Insights.hs:1889`). External RPC calls are outside this publication transaction.
4. Publication calls `competitionIsMutableForUpdate` (`Database/Insights.hs:1914`), which selects the competition row `FOR UPDATE` (`Database/Insights.hs:2084`).
5. Publication writes snapshot rows and batch metadata, then calls the same full integrity refresh before committing (`Database/Insights.hs:1963`). Thus a normal live snapshot cycle recalculates integrity twice, and the second calculation extends the competition lock.
6. `createRegistrationSession` inserts an application referencing `insights_competitions(slug)` (`Database/Insights/Registration.hs:574`). Its foreign-key key-share lock conflicts with the publication's `FOR UPDATE` lock.
7. Completion also explicitly locks the competition row (`Database/Insights/Registration.hs:1121`), so this design can delay the final registration step as well.

PostgreSQL holds these row locks until transaction end. `FOR UPDATE` conflicts with `FOR KEY SHARE`; `FOR NO KEY UPDATE` does not. See [PostgreSQL row-level locking documentation](https://www.postgresql.org/docs/current/explicit-locking.html#LOCKING-ROWS).

## Why the integrity query is expensive

The plan reveals several scaling problems:

- The `invalid_inbound` branch merge-joins transfers and participants on only `(chain_id, release_router)`. Wallet and token equality are residual `LOWER(...)` filters. With many participants on one release, this creates broad transfer/participant comparisons before filtering by wallet.
- The `premature_outbound` branch uses a transfer primary-key scan constrained only by chain and router, then filters sender and token addresses. It cannot use the address portions of the existing inbound/outbound indexes for those `LOWER(column)` predicates.
- Materialized `flow_rows` and `canonical_mints` are each estimated at one row. Correlated scans and join choices based on those estimates warrant targeted runtime profiling.
- The refresh updates all matching participant rows and `updated_at`, even when flags have not changed.
- The worker repeats this full-history computation instead of limiting work to changed inputs.

At inspection the table statistics estimated about 43,651 transfer rows, 73,113 account activity rows, and 17.1 million retained account snapshot rows. These are whole-table statistics, not exact counts for this competition. The baseline snapshot lookup uses the existing batch index; the captured plan does not show a full scan of the 17.1 million snapshots for that lookup.

A read-only SELECT analogue of the calculation, with merge joins disabled only in the diagnostic session, exceeded its 10-second statement limit. This rules out treating that planner setting as a demonstrated quick fix. It does not establish an exact runtime or per-node timing for the production UPDATE. No production UPDATE was executed by this investigation.

## Repair direction

1. Keep snapshot publication's competition/roster lock limited to the consistency check and atomic publication. Compute expensive integrity results outside that critical section, with an explicit input generation/cursor and validation before publishing results. Preserve registration, roster-change, reorg and finalization invariants.
2. Make participant listing a read. Schedule one integrity refresh for the relevant input changes rather than implicitly refreshing before a second refresh during publication.
3. Rewrite broad address joins to use normalized keys or appropriate expression indexes; replace repeated correlated scans with preaggregated keyed joins. Validate equivalent flags against existing integrity fixtures, including ambiguous transfer provenance and close-assistance cases.
4. Consider a narrower `FOR NO KEY UPDATE` publication lock where invariant analysis permits it. This can let initial registration foreign-key checks proceed, but is not a complete fix: explicit completion locks still conflict and the slow calculation remains.
5. Add bounded database lock/statement deadlines with retryable API errors and frontend recovery. These bound the impact; they do not replace the transaction/query repair.
6. Add a concurrency regression test showing registration starts promptly during snapshot publication and another that preserves roster/snapshot consistency during completion. Measure publication lock duration separately from integrity calculation duration.

## Investigation scope

No application code, database data, service configuration, or live worker settings were changed. Temporary diagnostic tasks used the already deployed image, only the existing database secret, read-only SQL sessions, and bounded statement/lock deadlines. A non-executing EXPLAIN and a bounded read-only SELECT were used instead of rerunning the production mutation. The temporary task definitions were deregistered after task completion.
