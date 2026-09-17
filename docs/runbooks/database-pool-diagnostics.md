# API database pool diagnostics

These diagnostics distinguish connection-pool starvation from slow SQL and
PostgreSQL lock contention. They do not increase the ten-connection pool, change
application query deadlines, terminate blockers, or enable paid AWS monitoring.

## Signals

`withDb` records pool-local counters in memory, using a monotonic clock. Source
labels are compiler-supplied module/line locations, not request paths or IDs.
Each connection's PostgreSQL backend PID is obtained once, when it is opened.

The API observer checks every five seconds. It emits `db_pool_observation` once
per minute normally, or at each check when there are queued requests, a recorded
wait of at least 250 ms, or a connection held for at least one second. This is a
warning threshold, not a request limit. Long legitimate background operations
may also trigger it.

The nested `observation` contains:

- Current `waiting` and `in_use`, plus active backend PIDs, source locations and
  `held_ms`. These survive counter resets and can identify an ongoing stall.
- Interval `acquired`, `released`, `failures`, and `abandoned_waits` counters.
  Failures include cancellation; they are not HTTP error counts.
- `wait_total_ms`, `wait_max_ms`, `hold_total_ms`, `hold_max_ms`, and the source
  of each maximum. Wait includes connection creation, and abandoned waits count.
  Hold measures time in the callback, including external work while it owns a
  connection; it is not SQL execution time. A completed duration belongs to the
  interval in which it is recorded, even if it began in an earlier interval.

On each emission, `db_blocking_snapshot` uses a **separate connection**, so it
does not queue behind the saturated application pool. This temporarily adds at
most one connection per API instance. Connection establishment has a two-second
libpq timeout; diagnostic SQL has a 1.5-second statement timeout and 500 ms lock
timeout, within a three-second application deadline. Session settings apply
only to that separate read-only diagnostic connection. No SQL text, parameters,
database URLs, usernames, application names, wallet addresses, request bodies,
or exception text are collected.

The snapshot includes up to 20 relevant sessions: current pool holders, blocked
sessions and their blockers, idle transactions, and active queries older than
one second. Each has a PID, state, wait-event type/name, query/transaction age,
and up to 20 blocking PIDs. API pool holders are prioritized at the row cap;
`truncated` indicates the row cap and `blocking_pids_truncated` marks a capped
blocker list. Query age is not
execution time for idle sessions. `cross_role_visibility=false` means the
database role lacks `pg_read_all_stats` visibility: an empty snapshot is **not**
proof that other database roles have no blockers. This change grants no roles.

Diagnostic failures/timeouts produce rate-limited `db_diagnostics_failed` or
`db_diagnostics_timeout` events. The observer retries; API requests do not fail
because diagnostics failed. Samples are periodic, not a complete trace, and a
blocker can disappear between the pool observation and the database snapshot.
Use CloudWatch for the full nested diagnostic records. The existing PostHog
projection does not export these nested fields; do not treat missing PostHog
details as absence of pool pressure or blocking.

## Incident interpretation

1. High `wait_max_ms` plus queued requests and held connections points to pool
   pressure. Match the oldest holder's `backend_pid` and static `source` to the
   database snapshot and deployed source revision.
2. Nonempty `blocking_pids` identifies a PostgreSQL blocking relationship. Check
   the blocking session's transaction age/state. Do not kill it automatically.
3. A long-held connection whose database session is idle suggests application
   work while holding a connection; inspect that source location. An active
   session with a large query age suggests SQL work or a database wait.
4. Compare existing candle/history `plether_db_pool_wait_ms` with query timing.
   Do not interpret low RDS CPU or disk latency as evidence against pool pressure.

Roll out with the normal backend deployment after review. Verify the two events
arrive within a minute and confirm visibility and bounded overhead. There are
no schema migrations, new alarms, or AWS settings to apply for this change.

## Log record integrity

The shared structured logger materializes each JSON record and its newline
before writing, and serializes complete writes/flushes per output stream. This
prevents concurrent requests from producing concatenated JSON objects and empty
lines, which would hide events from structured CloudWatch/PostHog queries.
stdout and stderr have independent locks; no cross-stream ordering is promised.
This covers calls through `Plether.Logging`, not arbitrary direct stdout writes,
process termination or downstream collector delivery. Sink backpressure can
still block logging; this change does not introduce a queue or drop policy.

## Local verification

Run `cabal test plether-api-test` in `apps/backend`. The observation tests cover
timing, failures, cancellation, pool isolation and privacy. Against an isolated
local PostgreSQL database, run:

```sh
PERPS_CRITICAL_PATH_DATABASE_URL=postgresql://localhost/plether_critical_path_test \
  cabal test plether-api-integration-test \
  --test-options='--match=diagnostics' --test-show-details=direct
```

The integration tests create a real advisory-lock blocker and saturate all ten
application connections while inspecting them over the independent connection.
They also verify read-only/deadline isolation, SQL timeout and cancellation
cleanup, and resource-pool discard behavior after a failed callback. Use only a
disposable local test database; never point this suite at production.

The logging regression test exercises 16 concurrent writers with small and
multi-chunk records on both stdout and stderr. To exercise multiple capabilities:

```sh
cabal test plether-api-test --test-show-details=direct \
  --test-options='--match=Plether.Logging +RTS -N4 -RTS'
```
