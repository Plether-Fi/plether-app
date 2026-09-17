# Insights database isolation and oracle recovery

## Incident evidence

On 2026-09-17 a short-lived, read-only ECS probe matched the running SQL prefix
to `fundingIntegrityRefreshSql`. The query was still active after 100 seconds.
The observed relation estimates were approximately 2,686 participants,
72,711 account activities, 42,813 transfers, and 17 million historical snapshots.
Publication holds the competition row lock until the funding check commits;
registration needs that row too. Low registration traffic is enough to consume
the old shared pool while a single long-running publication holds the lock.

Both probe tasks exited successfully and their temporary task definitions were
deregistered. No production `EXPLAIN ANALYZE`, data changes, session termination,
or service restart was used for diagnosis.

## Boundaries

- API capacity stays at ten application connections: seven general, two
  registration, one cached-Pyth. Diagnostics use at most one additional,
  short-lived read-only connection, sequentially across all three pools.
- Registration/cached-Pyth acquisition has a 250ms deadline; each checked-out
  callback has a two-second deadline. Connections use 250ms lock and one-second
  statement timeouts. Failed/cancelled leases are discarded.
- Insights publication, invalidation, boundary changes, participant refresh,
  eligibility review and finalization use transaction-local 250ms lock and
  1,500ms statement timeouts, plus a two-second application deadline.
- Insights transactions prefer hash joins over the observed correlated-key
  merge/nested-loop plans, with JIT disabled. These settings reset at transaction
  end; general API and registration sessions are not changed.
- A timeout rolls back the transaction and skips the cycle. Snapshot rows,
  batch metadata and integrity flags still commit atomically. Finalization
  continues to fail closed; no partial results become official.
- Transfer and activity matching is grouped once, retaining exact block,
  transaction, token, amount, direction and log-index evidence. Missing and
  duplicate matches remain invalid. Unchanged integrity flags are not rewritten.
- No snapshot retention change, table deletion, schema migration, additional
  AWS service, or increase in the combined API connection limit is included.

Registration returns `503 REGISTRATION_BUSY` with `Retry-After: 2`. The Insights
UI keeps consent and verified steps, refetches the session after an ambiguous
completion failure, and never automatically replays the completion POST.
Leaderboard freshness comes from the displayed rows' actual snapshot timestamp,
not HTTP fetch time. Live snapshots over three minutes old display a delay notice.

The oracle worker checks a fresh chain head after fetching the payload and again
immediately before simulation. Failed/stale attempts back off 5/10/20/30 seconds
(subject to the configured health polling interval). Strict future-time and age
limits remain intact. An existing pending transaction is reconciled before any
new submission.

## Verification

Use only a disposable PostgreSQL database whose name contains `critical_path`:

```sh
PERPS_CRITICAL_PATH_DATABASE_URL=postgresql:///your_critical_path_database \
  cabal test plether-api-test plether-api-integration-test --offline
```

Run from `apps/backend`. Integration fixtures create/delete synthetic rows and
must never run against production. The differential fixture freezes the original
funding SQL and compares integrity flags for the existing provenance, duplicate,
funding-substitution and close-assistance cases.

Opt-in capacity check:

```sh
INSIGHTS_PERFORMANCE_TEST=1 \
PERPS_CRITICAL_PATH_DATABASE_URL=postgresql:///your_critical_path_database \
  cabal test plether-api-integration-test --offline \
  --test-options='--match=2700' --test-show-details=direct
```

This creates 2,700 participants, 70,000 activities (5,400 funding events and
64,600 position events), 43,000 transfers, 270,000 unrelated historical snapshot
rows and twenty advancing snapshot batches.
`INSIGHTS_PERFORMANCE_ALL_FUNDING=1` instead makes all 70,000 activities funding
events as a separate stress case. `INSIGHTS_PERFORMANCE_EXPLAIN=1` enables a
30-second, rolled-back local `EXPLAIN ANALYZE` for diagnosis only.

These fixtures are not an end-to-end 10,000 transactions/day certification.
They do not reproduce the production hardware, full historical snapshot table,
RPC latency, transaction bursts, or provider quotas. Successful publications
under the deadline and production lock-hold p95 below 500ms are separate gates;
a passing timeout/rollback test alone does not demonstrate sufficient capacity.

Local PostgreSQL 16 results on 2026-09-17: all twenty mixed-workload publications
completed under two seconds; whole-publication p95 was **1,290ms**. This is still
above the 500ms target, so that performance gate remains open. Earlier all-funding
stress runs exceeded the deadline and correctly rolled back. Do not interpret
the passing mixed-workload check as a guarantee for that extreme workload or
for the full production database. No production rollout has been performed.

Also run the Insights tests/build, trading frontend tests/build, oracle worker
tests and Cloudflare Worker tests. Keep the unrelated crash-recovery Storybook
changes outside this implementation's commit.

## Rollout and acceptance

Roll out only a reviewed, merged master revision through GitHub Actions using
the repository's CLI authentication and duplicate-run checks. Never deploy this
worktree directly or bypass the protected backend environment.

1. Deploy the API first with `deployment_scope=api`, `environment=sepolia`,
   `bootstrap=false`. Confirm health/readiness and all three pool names in logs.
2. Deploy the backend's remaining worker containers with the same revision.
   The oracle updater and Insights worker are part of the backend rollout.
3. Verify the separate `Deploy Insights` workflow for the UI changes. It normally
   runs on merge of `apps/insights` changes; it is backward-compatible if it
   precedes the backend. Do not confuse it with the trading frontend deployment.
4. Inspect `insights_db_operation_completed` (operation, duration, reason),
   `insights_snapshot_cycle_skipped`, `db_pool_observation` and
   `db_blocking_snapshot`. None of these new logs contains raw SQL or credentials.
5. Require advancing committed snapshot timestamps, bounded registration
   failures followed by successful manual retry, healthy cached-Pyth responses,
   advancing oracle mark time and reconciliation of already-pending hashes.
6. Compare API p95, ALB target 5xx, pool waiting/hold time and RDS CPU/I/O before
   and after rollout. Do not declare success if trading is protected but every
   Insights cycle times out. Validate the 500ms lock-hold p95 target in the real
   environment; do not lengthen deadlines simply to pass that check.

There is no destructive migration to reverse. Rollback uses the previous
reviewed backend/Insights revisions through the normal deployment workflow.
Note that reverting the API also removes pool isolation, so an old long-running
Insights worker can again starve requests. Never disable integrity validation
or overwrite final competition state as a workaround.
