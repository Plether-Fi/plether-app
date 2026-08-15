# ADR 0001: Incremental Perps OHLCV Rollups

- Status: Accepted
- Date: 2026-08-12
- Owners: Backend and frontend platform teams
- Scope: Perps basket price history and market volume history

## Context

The legacy basket-history endpoint builds candles at request time. When the
requested interval is not stored directly, PostgreSQL scans minute snapshots,
groups them using a timestamp expression, sorts them, and selects the last
sample in each bucket. It separately scans raw account activity to aggregate
volume. A cold 30-day/hourly request has taken several seconds even though the
result contains only a few hundred points. Edge caching improves the common
case, but cache misses and refreshes still pay the full database cost.

The frontend also derives pseudo-OHLC values from close samples. That cannot
recover intra-minute high and low values and makes chart correctness depend on
client implementation details. The existing FX-derived price series naturally
has weekend gaps while the chart currently advertises a 24x7 session.

We need bounded, predictable history reads for every supported chart
resolution without concealing data quality or market-calendar semantics.

## Decision

We will maintain a versioned PostgreSQL read model of price candles and volume
rollups. Request latency will scale with the number of returned candles rather
than the number of source observations or account-activity rows.

The canonical resolutions are:

| Label | Seconds |
| --- | ---: |
| 1 minute | 60 |
| 3 minutes | 180 |
| 5 minutes | 300 |
| 15 minutes | 900 |
| 30 minutes | 1800 |
| 1 hour | 3600 |
| 1 day | 86400 |

All buckets are UTC/Unix-epoch aligned half-open intervals:
`[bucket_start, bucket_start + interval_seconds)`.

### Price-series semantics

- Candles describe admitted Pyth basket reference observations.
- The on-chain mark is exposed separately and does not silently replace a
  candle's close.
- The canonical event time is the minimum component publish time after the
  existing component-divergence checks pass. Minimum and maximum component
  publish times remain available for audit.
- At one event time, observations from the highest available source-priority
  tier are retained and lower-priority fallbacks are ignored. All observations
  tied in that winning tier remain distinct inputs.
- Open and close are the first and last retained observations in event-time
  order, with the stable observation identifier as the tie-breaker.
- High and low include every retained admitted observation in the bucket.
- Empty FX weekend buckets are absent. We will configure the chart session to
  match source availability instead of creating unmarked synthetic candles.

Prices are persisted in the raw contract/oracle domain as eight-decimal
integers. For display cap `K`, the API or client transform is decreasing:

```text
display_open  = K - raw_open
display_high  = K - raw_low
display_low   = K - raw_high
display_close = K - raw_close
```

The high and low swap is mandatory. Out-of-domain source values are rejected
or quarantined, never silently clamped.

### Basket definitions

A basket definition is immutable and identified by a canonical configuration
hash. It includes feed identifiers, weights, inversions, base prices, price
cap, decimals, derivation version, and its effective time range. Every
observation and price candle references a definition. Configuration changes do
not splice incompatible data into one primary-key series or rewrite prior
history.

### Source observations and price rollups

Each admitted signed update is deduplicated in an observation ledger before
minute information is lost. The writer recomputes the affected one-minute
candle and then all six parent resolutions in the same transaction. Parent
candles are derived only from one-minute candles:

- open: first non-empty child open;
- high: maximum child high;
- low: minimum child low;
- close: last non-empty child close;
- sample count: sum of child sample counts.

Recomputation, rather than blind increments, makes retries, duplicates,
out-of-order observations, and corrections idempotent. A correction updates
the affected minute and all parent buckets and advances the dataset generation
used for cache and cursor validation. Publishing a backfill tranche that
extends coverage also advances the generation, because the terminal page and
its pagination metadata can change even when existing candle values do not.

Legacy snapshots cannot reconstruct real historical OHLC. Backfilled
one-minute rows therefore use `O=H=L=C=sample`; larger rows aggregate those
samples. They are explicitly marked `legacy_sampled`, while candles derived
from the observation ledger are `observed` and mixed parents are `mixed`.

### Volume rollups

Volume is deployment-specific and remains separate from the global price
series. The current release router exposes one canonical DXY market, so the v1
identity includes chain, release router, resolution, and bucket start. A
multi-market router must add an explicit market identifier in a new derivation
version before it can share this read model.

The stored value is the exact numerator of the existing contract-notional
definition, based on `ABS(size_delta) * execution_price`. Division and flooring
occur only at the API boundary. Parent buckets sum exact one-minute numerators,
which prevents repeated rounding loss.

Indexer batches collect every touched minute and recompute those one-minute
volume rows from canonical account activity, followed by their parents.
Replayed events therefore cannot double count. Reorg handling marks affected
coverage incomplete, removes or recomputes the old-fork rollups, replays the
canonical fork, and only republishes coverage after the safe watermark catches
up.

### Finalization, coverage, and generations

Price and volume have separate completeness watermarks:

- price completeness follows the source watermark plus a configurable
  lateness window;
- volume completeness follows the canonical indexer's confirmed block/hash
  watermark.

The price writer becomes eligible to finalize a bucket only after the source
lateness window. Because durable publication runs on an asynchronous polling
loop, strict readers apply a separate bounded finalization-publication grace
(15 seconds by default) before requiring the newly eligible aligned watermark.
During that grace they still clip every response to the previously stored
`finalized_through`; no unfinalized row becomes visible. Coverage freshness
does not receive this grace, and a frozen finalizer still fails closed when the
bounded window expires. The 60-second configuration cap ensures the grace can
never relax freshness by more than one canonical candle bucket. Terraform
refuses rollup reads unless the grace is at least five seconds longer than the
configured basket-writer poll cadence.

The supervised frozen-finalizer canary holds the exact price-dataset writer
lock across one hourly eligibility boundary without changing coverage data.
The supervising operator issues the three cache-bypassed current-candle
requests; the canary executable only controls and observes database state. Its
2,100-second application deadline, 2,400-second workflow task deadline,
12-second idle-transaction fail-safe, and 90-minute job limit bound the fault
while preserving post-deadline cleanup time.
The supervised Gate control passes only after the operator proves the expected
request fails closed, the lock is released on schedule, price advances from
the previous hour to the boundary while volume remains healthy at or beyond the
boundary, both generations remain unchanged, and the same request succeeds
again. Workflow/task success alone proves only the database lock and recovery
lifecycle; it does not prove that the three HTTP observations occurred.

A historical page is eligible for rollup reads only when the complete requested
range is covered for both required sources and the active derivation version.
The presence of one row is not proof of coverage. Partial backfills must never
produce sparse production pages.

Corrections to finalized data increment the relevant row revision and dataset
generation. Current candles are mutable replacements; finalized history pages
are generation-bound.

### Read API

The backend exposes:

```text
GET /api/perps/basket/candles?interval=<seconds>&cursor=<page-boundary>
GET /api/perps/basket/candles/current?interval=<seconds>
```

Only canonical resolutions are accepted. Historical pages contain finalized
candles in strictly ascending order. The server reads at most `limit + 1` rows
from indexed rollup keys to determine whether earlier history exists.

The first implementation uses deterministic page boundaries aligned to
`interval_seconds * 500`. Each response binds its contents to a dataset
generation; clients reject mixed generations, clear cached pages, and restart
pagination after a correction. Page traversal counts actual candles rather
than theoretical time buckets, so weekend gaps do not reduce TradingView
`countBack`, and one traversal has a fixed 24-page request budget. The initial
page is clamped to the browser's current containing page, while the server
allows only one adjacent future page to tolerate clock skew at a boundary. A
future API version may encode the series, scope, resolution, and generation
into an opaque cursor without changing storage semantics.

The current-candle endpoint returns a full OHLCV replacement with revision and
source completeness. It does not participate in immutable historical
pagination.

The legacy `/basket/history` endpoint remains available during rollout and is
served by one bounded rollup range read once complete coverage exists. Direct
raw-table fallback remains bounded and is removed after the compatibility
window. Per-component point metadata is not part of the rollup model, so the
legacy component response is restricted to its actual UI use case:
`range=24h`, `interval=3600`, and `includeComponents=true`.
That compatibility response performs only the bounded snapshot query. Its
per-point volume is deliberately zero and non-authoritative; the market-stats
endpoint remains authoritative for rolling 24-hour volume. The browser reuses
the component response on a five-minute refresh cadence.

Closed edge-cache entries are keyed internally by the authoritative current
dataset identity: series, configuration hash, display-price cap, and positive
dataset generation. The worker checks that identity directly at origin before
serving a historical cache entry and caches a newly fetched page only when its
identity matches the probe. A failed or incomplete probe bypasses the shared
page cache. Thus a correction or reorg makes an old generation unreachable
even for a first-time client that has no in-memory generation baseline.

### Tables and indexes

The read model consists of:

- `perps_basket_definitions`;
- `perps_basket_observations`;
- `perps_basket_candles`;
- `perps_market_volume_rollups`;
- `perps_rollup_coverage`.

Price identity is `(series_id, interval_seconds, bucket_start)`.
Volume identity in the single-market v1 deployment is `(chain_id,
release_router, interval_seconds, bucket_start)`. These primary-key orders
support the complete history-page range scans. Raw events receive a time-bounds
index, raw account activity receives a filtered covering index for backfill and
repair, and both source tables receive block-number indexes for bounded reorg
discovery and deletion; serving requests never scans either source table.

Schema changes use an explicit, idempotent additive migration command. The
event-bounds, covering activity, and block-rewind indexes are created
concurrently outside a transaction; runtime startup functions do not perform
historical rewrites or index builds.

### Administration

The `plether-candle-admin` executable provides:

```text
estimate
migrate
backfill price|volume|all
status
verify
repair price|volume|all
finalizer-probe --boundary <aligned-hour>
```

Backfill and repair run on a single database connection under a PostgreSQL
advisory lock. They use bounded, independently committed chunks, configurable
lock and statement timeouts, optional throttling, an absolute application
runtime, and durable coverage state. Both operations require dual-write mode;
the final repair reconciliation and publication use a repeatable-read canonical
source snapshot, and an empty canonical source domain cannot be published as
complete coverage. Backfill runs newest-first so useful chart windows become
available first. It reads only existing PostgreSQL history and never calls
remote Pyth services. Coverage is extended only across adjacent completed
chunks.

`finalizer-probe` is a Sepolia-only, hourly canary for the strict read failure
path. It accepts one aligned boundary and requires the exact active `3600`
rollout configuration. After taking the global candle-admin lock, it acquires
the same transaction-scoped price dataset advisory lock as the live writer in
a read-committed, read-only transaction; this makes state committed by a writer
that held the lock visible after acquisition. Recovery is sampled in a fresh
repeatable-read, read-only snapshot. The probe performs no row mutation, its
transaction lock releases automatically on commit, rollback, disconnect, or
task termination, and it exits successfully only after price reaches the
protected boundary, volume remains healthy at or beyond it, and neither
dataset generation changes.

A bounded repair first captures each selected dataset's complete per-interval
coverage envelope and records a durable maintenance identity binding that
envelope to the exact requested `[from, to)` range. Every selected kind is then
marked incomplete before any independently committed rebuild chunk runs. Final
publication acquires the writer locks, revalidates the marker, generation,
requested bounds, and preserved envelope, reconciles the repaired range against
canonical sources, and atomically restores the prior coverage envelope for all
selected kinds. Thus a repair does not shrink valid outside-range coverage, but
it also cannot reassert the envelope until the bounded work has passed final
validation. If repair stops after invalidation, a retry is accepted only for the
identical range while every relevant interval still carries that consistent
maintenance identity; a concurrent reorg, watermark invalidation, different
range, or any other incomplete state aborts publication and fails closed.

Controlled duplicate-ingestion testing uses the Perps indexer's separate
bounded replay mode, never its legacy `--backfill` invocation. Replay is
Sepolia-only, requires explicit inclusive block bounds of at most 5,000 blocks,
and executes in one transaction with statement, lock, application, and workflow
deadlines. It disables remote evidence enrichment and does not advance or
rewind the canonical cursor, advance coverage, or certify canonical progress.
The protected workflow accepts replay only when exactly one stable indexer
topology (consolidated XOR standalone) is running in dual-write mode.

For ordinary candle administration, the GitHub workflow snapshots the stable
API service's task definition, network configuration, image repository, and
unanimous running-task image digest. It preserves the deployed execution
settings and all sidecars and changes only the API image to
`repository@sha256:digest`. For replay, it snapshots the selected stable writer
service, pins the unanimous running Perps indexer digest, rejects dependencies
on excluded containers, and derives a task containing only the unchanged
indexer definition and its exact FireLens sidecar. The workflow registers the
derived revision in a dedicated admin family, tags it with the workflow-run
owner, and launches that exact revision with a unique ECS `startedBy` identity
and idempotent client token. The finalizer probe also validates the exact
merged/deployed SHA, Sepolia chain and database, one complete writer topology,
writer/read modes, hourly allowlist, strictness, lateness, and grace before
pinning both the API and FireLens digests. It persists and validates the task
ARN before exposing outputs and verifies the selected container's digest.
`always()` cleanup recovers the task and definition by their immutable
identities, confirms the task is stopped, and confirms the owned revision is
inactive. If a runner is lost before cleanup, the application deadline bounds
the process and the next run rejects the stale family until it is reconciled.
An application deadline remains effective even if the runner itself is lost.
This prevents a mutable registry tag from changing what an approved
administration run executes or leaving an unbounded mutator behind. The admin
workflow and backend deployment share environment-specific mutation
concurrency; the workflow also uses protected GitHub environments. Inputs are
strictly allowlisted, logged without credentials, and require an explicit
environment confirmation phrase. Both `candle-admin-sepolia` and
`candle-admin-mainnet` must have at least one required reviewer, require an
explicit approval, disable administrator bypass, and permit only `master`. The
supervised Sepolia rollout may use its sole administrator's self-approval while
no independent reviewer is available, so `prevent_self_review` is false there;
mainnet still requires an independent reviewer and sets it to true.
The first rollout follows the repository's existing environment-scoped AWS
credential convention with a commit-pinned credential action. Migrating this
privileged workflow to a narrowly scoped GitHub OIDC role is a follow-up once
the required IAM trust and environment role are provisioned.

### Runtime and rollout controls

Runtime behavior is gated independently:

```text
PERPS_CANDLE_WRITE_MODE=off|dual
PERPS_CANDLE_READ_MODE=legacy|shadow|rollup
PERPS_CANDLE_READ_INTERVALS=<comma-separated seconds>
PERPS_CANDLE_SHADOW_SAMPLE_BPS=<0..10000>
PERPS_CANDLE_LATENESS_SECONDS=<seconds>
PERPS_CANDLE_FINALIZATION_GRACE_SECONDS=<0..60>
```

Safe defaults are writes off and legacy reads. The rollout order is:

1. deploy compatible code with rollup writes and reads disabled;
2. apply the additive migration;
3. enable dual writes, perform a protected bounded Sepolia replay, and soak;
4. backfill newest-first and verify coverage and reconciliation;
5. repeat deterministic source-to-rollup reconciliation after a dual-write
   soak;
6. canary rollup reads by interval;
7. enable the frontend candle client;
8. retain legacy storage and reads through the rollback window.

Configuration validation rejects rollup reads or a non-empty interval
allowlist unless write mode is `dual`. This prevents a deployment from serving
an apparently immutable read model while canonical writers have stopped
maintaining it.

`shadow` read mode and its sample-rate setting are reserved compatibility
controls for a future bounded comparator. They do not compare responses or
switch traffic in v1 and are not a prerequisite for the v1 rollout.

Rollback is flag-based. Operators disable the frontend flag and switch backend
reads to `legacy`; they do not drop tables or delete source data during an
incident.

## Invariants

The implementation and database constraints enforce:

- one row per series, interval, and aligned bucket;
- `raw_low <= raw_open, raw_close <= raw_high`;
- nonnegative sample count, trade count, and volume numerator;
- observed price candles have a positive sample count;
- parent aggregation is identical to direct aggregation;
- duplicates and input order do not affect output;
- complete coverage is required before rollup reads;
- pagination has no overlap or omission;
- reorg repair leaves no stale volume in child or parent buckets;
- request-path SQL uses only rollup primary-key range scans.

## Performance acceptance criteria

With production-like data and at least twice observed peak traffic:

| Metric | Target |
| --- | ---: |
| Combined rollup SQL p95 / p99 | <= 50 ms / <= 100 ms |
| Backend application p95 / p99 | <= 150 ms / <= 300 ms |
| Direct-origin HTTP p95 / p99 | <= 750 ms / <= 1 s |
| Edge hit p95 | <= 150 ms |
| Initial chart history p95 | <= 1 s |
| Current-candle lag while live | <= 10 s |
| Source lateness | 2 min |
| Finalization publication after eligibility | <= 15 s |

Apply the first three latency targets independently to every canonical interval
and successful canonical request shape. Do not pool endpoints, intervals,
current candles, active pages, or closed or inception-clipped pages. For native
candle responses, read SQL and application durations from the
route-specific `Server-Timing` metrics `plether_db_candles` and `plether_app`.
The transitional compatibility-history route is excluded from rollup latency
acceptance because its remaining consumers are migrated separately before the
route is removed. Its bounded-shape and zero account-activity-scan invariants
remain correctness checks until that removal. `plether_app` already includes
database time and must not be added to a database metric.

Before recording each series, issue 10 fixed, unrecorded warm-up requests.
Evaluate direct-origin HTTP using established persistent keep-alive connections,
with HTTP/2 where negotiated, from a documented probe location while the load
condition above is present. Measure fresh DNS/TCP/TLS connections separately,
report their phase timings, and never mix cold-transport observations into the
warm series. Calculate nearest-rank percentiles from at least 200 recorded
observations for p95 and 1,000 for p99; a pass asserting both therefore requires
at least 1,000 observations per request shape. Record failures separately and
do not silently retry, discard, or replace them.

In addition, a normal initial chart load should require no more than two
history pages, each SQL page should inspect at most its bounded page size, and
rollup mode must execute no raw snapshot or account-activity scans.

## Consequences

### Positive

- History latency is bounded and predictable for all supported resolutions.
- The API returns real server-derived OHLCV rather than client pseudo-candles.
- Retries, corrections, and reorgs have explicit idempotent repair paths.
- Data quality, basket changes, weekend gaps, and historical limitations are
  visible instead of being silently normalized.
- Edge caching becomes load smoothing rather than a latency requirement.

### Costs and risks

- Writers perform additional transactional work and storage grows with the
  observation ledger and seven resolutions.
- Migration's concurrent source-table index builds can be long and can wait on
  catalog locks, so they use bounded statement and lock timeouts. Candle data
  backfill remains a separate rollout action.
- Finalization, coverage, generation, and reorg semantics add operational
  complexity.
- Historical OHLC quality remains limited to sampled closes before the
  observation ledger was enabled.

These costs are accepted because request-time aggregation cannot meet the
latency objective reliably and cannot provide correct OHLC values.

## Alternatives considered

### Keep request-time SQL and add indexes

Rejected as the durable solution. Expression grouping, sorting, and volume
aggregation still scale with raw history and remain vulnerable to cold-cache
latency. A covering index is useful for migration and repair only.

### Rely on edge or backend response caching

Rejected as the primary fix. Cache misses, expirations, corrections, and
stampedes still execute the expensive query, and cache state does not solve
OHLC correctness.

### PostgreSQL materialized views

Rejected for the initial implementation because native refresh is not an
incremental per-bucket mechanism and can require large refresh work or bespoke
concurrent refresh coordination.

### TimescaleDB continuous aggregates

Deferred. It could provide continuous aggregates, but introduces an extension
and operational dependency that is not currently provisioned. Application-
maintained rollups are sufficient at the present scale and keep the schema
portable.

### Generate all candles in the frontend

Rejected. It transfers excess history, duplicates derivation logic across
clients, cannot recover discarded observations, and provides no reusable
volume or correction model.

## Follow-up

- Keep the concrete table definitions and idempotent operational migration
  procedure synchronized with the schema implementation.
- Add property tests for aggregation, boundaries, duplicates, corrections,
  display-domain extrema, pagination, and exact volume arithmetic.
- Add PostgreSQL integration tests for resumable backfill, partial coverage,
  query plans, and reorg repair.
- Revisit observation retention after measuring storage for 30 to 90 days.
- Revisit a session-aware daily calendar only as a separately versioned series.
