# Registration lock repair rollout

## Release 1

Rollback image/commit: `4bf48ef62cd4a52882cc6338c27ce460a60c35ff` (PR #312).
Backend deployment: https://github.com/Plether-Fi/plether-app/actions/runs/35249374459
Insights deployment: https://github.com/Plether-Fi/plether-app/actions/runs/35249343957
Both workflows succeeded; public API status and Insights homepage smoke checks passed.
Local verification: 1,226 backend unit tests, 161 PostgreSQL integration tests,
114 Insights tests, frontend lint/build. GitHub checks also passed.

Registration transactions use transaction-local 1-second lock and 5-second
statement limits. SQLSTATE 55P03/57014 return 503 REGISTRATION_BUSY with
Retry-After: 2. Snapshot publication alone uses FOR NO KEY UPDATE. Requests
have a 30-second browser deadline including body reading. Ambiguous errors
reconcile with GET session before another mutation; POST is never replayed.

## Release 2 design

Participant listing is read-only. Snapshot publication writes complete batches
without calculating integrity. Each worker cycle calculates once with a
nonblocking competition-specific advisory lock, a 15-second statement limit,
and one CTAS SQL snapshot on its own connection-local temporary table.
Publication validates the captured input epoch and baseline under a short
FOR NO KEY UPDATE transaction (lock timeout 1 second, statement timeout 5 seconds).
Flags update only when different, and freshness metadata commits atomically.
Roster identity, baseline, release/boundary changes, and canonical history
rewinds invalidate the epoch. Forward indexing permits one-cycle live lag.

Approval/finalization also calculate before taking competition locks and reject
changed epochs/history identities. Finalization RPC proof is collected outside
the transaction and its exact target revalidated under history/competition locks.
Finalized materialized standings remain immutable; archived July SQL is unchanged.

`integrityStatus` is pending before any successful calculation, stale when its
epoch differs or its age exceeds twice the configured poll interval, otherwise
current. Finalized competitions are current. Optional checked time/block metadata
is additive. Stale rows keep P&L/rank but cannot receive provisional prizes and
report fundingIntegrityClear=false. Failed calculations preserve existing flags.

## Migrations and rollback

Apply `apps/backend/config/migrations/insights-integrity-epoch-v1.sql` in one
transaction first. Acquire the required table locks NOWAIT and retry outside the
transaction if old workers are still calculating; never kill the blocker.
Apply `insights-integrity-indexes-v1.sql` using psql with ON_ERROR_STOP outside a
transaction. Inspect pg_index.indisvalid for all three normalized indexes. If
an interrupted concurrent build left an invalid index, drop only that invalid
index concurrently before retrying. No concurrent index build runs at startup.

The additive epoch migration and all three valid expression indexes were applied
to Sepolia before Release 2, using the Release 1 image in a one-off migration task.
The first bounded attempt rolled back on contention; a NOWAIT retry succeeded.

Set `INSIGHTS_INTEGRITY_REFRESH_ENABLED=false` on the Insights worker to disable
background integrity publication while leaving snapshots and registration limits
running. Existing flags remain visible and become stale after the freshness window.
The API must stay on Release 2 to expose stale status; retain the Release 1 image
above for a full application rollback if necessary. Do not remove additive columns
or indexes during rollback. Do not increase pools or change global planner settings.

## Verification commands and measurements

Use a dedicated local database with `critical_path` in its name. Run backend unit
and integration suites with the built plether-candle-admin on PATH. Opt into the
larger fixture using `INSIGHTS_INTEGRITY_BENCHMARK=1` and Hspec match
`benchmarks isolated integrity`; never point this destructive test at live data.
The fixture has 2,702 participants, 162,001 activities, 91,802 transfers, and
over two million retained snapshots. It emits twenty samples and an actual-row-count
EXPLAIN ANALYZE plan at `/tmp/insights-integrity-benchmark-plan.json`.

Events: `insights_integrity_refresh` (calculation_ms, publication_ms, published),
`insights_integrity_publication` and `insights_snapshot_publication`
(lock_wait_ms, lock_held_ms), `insights_integrity_freshness` (age_seconds),
`insights_integrity_skipped`, `insights_integrity_failed`,
`registration_database_work` (duration_ms), and `registration_database_busy`
(sql_state). Snapshot lock-held metrics include commit and exclude loading/parsing the private
input staging table, which completes before locking. Integrity lock-held metrics end immediately before
commit; full integrity publication benchmarks include commit and conservatively
bound the complete lock duration. The benchmark also measures 400 concurrent
session creations and completion-recovery operations.
No events contain tokens, wallet identities, or personal data.

The incident's public participant count was 1,040. The acceptance fixture therefore
uses over 2.5 times that roster and more than twice the observed transfer/activity
volume. Each measured snapshot publication advances to a new block. An additional
5,402-participant stress fixture exposed excessive snapshot index I/O (0.7–3.5s
lock durations) and is not reported as a passing acceptance run. Snapshot publication
now preserves unchanged rows and only deletes obsolete wallets before upserting a
complete batch. Loading/parsing its private input table occurs before locking. New blocks use plain INSERT; replacement blocks use a changed-row UPSERT.
The activity calculation uses a
materialized, release-filtered activity set to avoid one-row cardinality estimates
that caused repeated full flow scans; the isolated original candidate took over
99 seconds. No global planner switches or pool sizes were changed. The bounded integrity
calculation disables JIT only transaction-locally: the isolated Linux plan showed
about five seconds of LLVM compilation on every refresh. Assistance evidence is
also grouped once and joined, preserving EXISTS/duplicate semantics. The archived
July calculation remains unchanged.

The first native benchmark used PostgreSQL 16 with its default 128 MB shared cache.
Its 2,702-participant run passed calculation (p95 3.92s), integrity publication
(36.5ms), and registration (3.3ms), but failed snapshot lock time (p95 639ms).
The final acceptance environment is a separate localhost-only PostgreSQL 16 ARM64
container limited to 2 CPUs and 4 GB RAM, matching Sepolia's db.t4g.medium class,
with a 1 GB shared cache. No live or existing local-server settings were modified.
No benchmark writes target Sepolia.

Release 2 deployment, benchmark acceptance, and ten consecutive observed worker
cycles remain rollout gates; append their measured results before completion.
