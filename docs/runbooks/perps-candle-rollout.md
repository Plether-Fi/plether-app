# Perps Candle Rollup Rollout

This runbook enables the incremental Perps OHLCV read model described in
[ADR 0001](../adr/0001-perps-candle-rollups.md). Run every GitHub Actions
operation with `gh`; repository-wide GitHub CLI guidance is in
[AGENTS.md](../../AGENTS.md).

## Safety rules

- Roll forward candle infrastructure and behavior one environment at a time:
  Sepolia before mainnet for Terraform candle configuration, migration, dual
  writes, backfill, rollup reads, and frontend feature activation. The
  repository's existing `master` push workflows may deploy this compatible,
  inert backend/frontend code to mainnet first; that is not a candle rollout
  gate. With no activation changes, backend defaults keep writes off, reads on
  legacy with an empty interval allowlist, and an absent frontend repository
  variable resolves the candle API flag to false.
- Keep `PERPS_CANDLE_WRITE_MODE=off`, `PERPS_CANDLE_READ_MODE=legacy`, an
  empty `PERPS_CANDLE_READ_INTERVALS`, and the frontend flag off until the
  corresponding gate below passes.
- Never enable public rollup reads with
  `PERPS_CANDLE_STRICT_COVERAGE=false`.
- Run database mutations only through `.github/workflows/candle-admin.yml`
  from `master`. Both `candle-admin-sepolia` and `candle-admin-mainnet` must
  have at least one required reviewer, require an explicit approval, disable
  administrator bypass, and allow only `master`. During the supervised Sepolia
  rollout, its sole administrator may self-approve when no independent reviewer
  is available, so `prevent_self_review` must be false there. Mainnet must still
  require an independent reviewer with `prevent_self_review` set to true.
- Backfill and repair are accepted only while every deployed candle writer is
  in `PERPS_CANDLE_WRITE_MODE=dual`. The admin workflow and backend deployment
  share one environment-specific concurrency group so a deployment cannot
  change write mode while either operation is reconciling canonical sources.
- The admin workflow must resolve the stable API service's deployed image
  digest, register a temporary task-definition revision whose API image is
  `repository@sha256:digest`, verify the started task reports that same digest,
  and deregister the temporary revision in cleanup. A commit-tagged deployed
  definition is not itself sufficient evidence that the one-off task pulled
  the deployed image.
- Before migration or backfill, record the RDS storage type, provisioned IOPS
  and throughput, `BurstBalance`, pending modifications, backup retention,
  deletion protection, final-snapshot configuration, free storage, CPU
  credits, and freeable memory.
- Stage RDS safeguards without overlap: (1) in a healthy maintenance window,
  create and verify a retained manual snapshot, accounting for the brief I/O
  suspension a Single-AZ snapshot can cause; (2) keep `db_storage_type` pinned
  to the live type while activating the pending positive backup-retention/PITR
  setting, wait for `available`, verify a restorable backup, and require an
  empty pending-modification set; (3) only then change `db_storage_type` from
  `gp2` to `gp3` in a newly refreshed, complete saved Terraform plan and apply
  that exact plan; (4) wait for storage optimization to finish, then record
  three consecutive healthy five-minute observation periods before running any
  candle admin action.
- `ApplyImmediately` (`apply_immediately`) activates every pending RDS change,
  not just the intended one. Review the complete pending-modification set
  first; changing retention from zero to a positive value takes the database
  offline while RDS creates the first automated backup. Never use a targeted
  or `-refresh=false` Terraform apply for these stages.
- For a supervised Sepolia storage conversion that must start before the next
  maintenance window, set `db_apply_immediately=true` only in the newly
  refreshed saved `gp3` plan and require an empty pending-modification set
  immediately before applying it. The exact plan may contain only the RDS
  `storage_type` change from `gp2` to `gp3`, provider intent changing
  `apply_immediately` from `false` to `true`, and rollout-guard bookkeeping;
  require zero creates and zero destroys, and reject class, allocation, network,
  secret, retention, or protection changes. This switch is rejected outside the
  safeguarded Sepolia `gp3` path. Terraform records operator intent but AWS does
  not expose `apply_immediately` as persistent instance state. After storage
  optimization finishes, restore it to `false` through another complete saved
  plan containing only that flag and rollout-guard bookkeeping; the provider
  still sends an RDS modify request with immediate mode disabled, so wait for
  `available` and an empty pending set before the cleanup apply. Then require a
  fresh final no-op plan. Terraform can return while RDS is still optimizing
  storage; completion still requires RDS `available`, `gp3`, an empty pending-
  modification set, and the three healthy observation periods.
- Mainnet Terraform must have a non-empty
  `operations_alarm_sns_topic_arn`, and the topic subscription must be
  confirmed and tested before backfill; the configuration now fails closed
  when the ARN is absent.
- Stop a backfill when an RDS pressure alarm fires, API p95 exceeds 750 ms for
  three periods, API 5xx errors increase, or replication/storage health is
  uncertain. A stopped chunk is safe to retry.
- Each one-off task has an application-enforced absolute runtime and a shorter
  workflow deadline. The workflow assigns a unique ECS `startedBy` identity,
  persists the task ARN immediately, recovers it by that identity if the
  runner is cancelled, stops unfinished tasks, and deregisters the temporary
  task-definition revision during cleanup.
- Do not drop legacy tables or raw observations during the rollback window.

## GitHub CLI setup

Authenticate and verify the target commit before every dispatch:

```bash
gh auth status
gh api user --jq .login
gh api repos/Plether-Fi/plether-app/commits/master --jq .sha
gh run list --repo Plether-Fi/plether-app --limit 20
```

Set a helper for the protected admin workflow:

```bash
run_candle_admin() {
  environment=$1
  action=$2
  scope=$3
  shift 3
  gh workflow run candle-admin.yml \
    --repo Plether-Fi/plether-app \
    --ref master \
    -f environment="$environment" \
    -f action="$action" \
    -f scope="$scope" \
    -f chunk_seconds=86400 \
    -f statement_timeout_ms=1800000 \
    -f lock_timeout_ms=5000 \
    -f throttle_ms=250 \
    -f confirmation="RUN ${action:u} ON ${environment:u}" \
    "$@"
}
```

The function uses zsh uppercase expansion. With another shell, pass the
workflow fields directly. After every dispatch, locate and watch the exact
run:

```bash
gh run list \
  --repo Plether-Fi/plether-app \
  --workflow candle-admin.yml \
  --event workflow_dispatch \
  --limit 10

gh run view RUN_ID \
  --repo Plether-Fi/plether-app \
  --json event,headBranch,headSha,status,conclusion,url

gh run watch RUN_ID \
  --repo Plether-Fi/plether-app \
  --exit-status
```

For the supervised Sepolia rollout, approve the pending deployment with `gh`;
do not use the Actions web UI. First read the pending environment and copy its
numeric ID, then submit the explicit approval (replace `ENVIRONMENT_ID`):

```bash
gh api \
  repos/Plether-Fi/plether-app/actions/runs/RUN_ID/pending_deployments \
  --jq '.[] | {environment_id: .environment.id, environment: .environment.name}'

gh api \
  --method POST \
  repos/Plether-Fi/plether-app/actions/runs/RUN_ID/pending_deployments \
  -F 'environment_ids[]=ENVIRONMENT_ID' \
  -f state=approved \
  -f comment='Supervised Sepolia candle rollout approval'
```

This is a required-reviewer approval, not administrator bypass. It is accepted
only for `candle-admin-sepolia`, whose policy deliberately sets
`prevent_self_review=false`; the mainnet environment must keep it true.

The environment-scoped AWS identity used by this workflow must be able to
describe the stable service and its tasks, register and deregister the
dedicated `plether-<environment>-candle-admin` task-definition family, run and
stop tasks, and pass only the API task and execution roles. The workflow copies
the deployed definition's execution fields and every container definition,
changes only the `plether-api` image, and never prints environment variables,
secret values, or the generated definition JSON.

The workflow passes an application deadline of 19,800 seconds, leaving thirty
minutes of the six-hour job budget for task/image verification and cleanup.
`plether-candle-admin` also accepts `--max-runtime-seconds` for local emergency
operation, but production mutations must continue to use the protected
workflow.

## Gate 1: compatible deployment

Merging compatible code to `master` may trigger the repository's existing
automatic mainnet backend/frontend deployments before Sepolia. This exception
is safe only while the candle behavior remains inert: writes off, legacy reads,
an empty interval allowlist, and the frontend candle flag false. Treat that
automatic image deployment as code compatibility only; it does not authorize
migration, dual writes, backfill, rollup reads, or frontend feature activation.

Before activating any candle behavior in either environment, set that target's
Terraform candle variables to the safe explicit values below, review the
complete plan, and apply it in the scheduled infrastructure window described in
the safety rules. This first apply registers the API and both candle-writer
topologies with one consistent configuration and installs the rollout guards
and operational alarms. Do this on Sepolia before the equivalent mainnet apply.
Only after that apply succeeds should an image-only backend deployment be used
for subsequent configuration gates; the deployment workflow deliberately
reuses the registered task-definition environment and does not inject these new
variables itself.

Before generating the complete plan, read and record the live RDS instance
class, allocated and maximum storage, storage type, provisioned IOPS and
throughput, `BurstBalance`, instance/storage-optimization status, pending
modifications, backup retention, and deletion protection. Set
`db_instance_class`, `db_allocated_storage`, and `db_storage_type` explicitly
from live state before the first transition plan; do not copy a desired-final
example over an existing instance. Reject a plan that replaces or deletes the
database, reduces its class or storage, regresses `gp3` to `gp2`, changes an
unrelated network or secret resource, or includes an unexplained pending RDS
modification. Refresh all state, review the complete plan, save it, and apply
that exact saved plan; do not use `-target` or `-refresh=false`. RDS storage
autoscaling can make the live allocation larger than an old Terraform
baseline. While autoscaling is
enabled, the pinned AWS provider suppresses any live allocation above the
configured baseline, including manual or other out-of-band increases. Compare
the live value explicitly, update the baseline instead of attempting a
downgrade, and do not add a broad `ignore_changes` rule that would also hide
intentional capacity changes.

The Terraform operator must be able to describe CloudWatch alarms and to put,
delete, list tags for, tag, and untag only the target environment's
`plether-<environment>-*` alarms. A refresh that cannot read existing alarm
tags is incomplete; fix the operator permission instead of planning with
refresh disabled.

Expected values:

```text
PERPS_CANDLE_WRITE_MODE=off
PERPS_CANDLE_READ_MODE=legacy
PERPS_CANDLE_READ_INTERVALS=
PERPS_CANDLE_SHADOW_SAMPLE_BPS=0
PERPS_CANDLE_STRICT_COVERAGE=true
PERPS_CANDLE_LATENESS_SECONDS=120
```

Pass criteria:

- the Terraform plan/apply succeeds with the safe candle values, and the active
  API, basket writer, and Perps indexer task definitions all expose exactly that
  configuration;
- the plan contains no database replacement, deletion, class downgrade, or
  allocated-storage downgrade, and the live RDS capacity and pending
  modifications were recorded before apply;
- any retention/PITR activation and `gp2` to `gp3` conversion followed the
  staged sequence above; storage optimization is complete and three
  consecutive five-minute periods passed with storage-pressure alarms OK,
  average read/write latency below 20 ms, API p95 below 750 ms, no increased
  5xx rate, and both health and legacy-history requests succeeding;
- backend deployment succeeds and `/api/health` returns 200;
- all stable API service tasks use one task-definition revision, one API image
  repository, and one deployed `sha256` image digest;
- the legacy basket-history endpoint still succeeds;
- candle endpoints return 404 while their intervals are disabled;
- API and worker error rates remain at baseline.

## Gate 2: estimate and migrate

Do not dispatch any candle admin action, including `estimate` or `status`,
while a `gp2` volume has exhausted `BurstBalance` or RDS reports storage
optimization in progress. Let burst balance recover or complete the `gp3`
sequence in Gate 1, then require its three consecutive healthy five-minute
observation periods.

Estimate work before changing the database:

```bash
run_candle_admin sepolia estimate none
```

Record source rows, estimated chunks, and the rollup row upper bound. Confirm
there is enough storage headroom for the source ledger, seven price
resolutions, seven deployment-volume resolutions, indexes, and normal RDS
growth.

Apply the additive migration:

```bash
run_candle_admin sepolia migrate none
run_candle_admin sepolia status none
```

The API or Perps indexer must have initialized `perps_events` and
`perps_account_activity` before this gate. The migration fails with an explicit
prerequisite error otherwise. It also detects the invalid catalog entries
PostgreSQL can leave after an interrupted concurrent index build, drops only an
exact invalid index, and retries before verifying the rebuilt index is valid.

Pass criteria:

- the migration task exits successfully;
- all five candle tables exist; the event-bounds and filtered account-activity
  backfill indexes and both block-rewind indexes are reported valid by
  PostgreSQL;
- no coverage record is marked complete merely because the schema exists;
- database locks, CPU, I/O latency, free storage, and foreground API latency
  remain healthy.

## Gate 3: dual writes

Set only `perps_candle_write_mode = "dual"` in the Sepolia Terraform variable
set and apply Terraform. This registers the task definitions with the new
environment before the image-only backend deployment workflow reuses them.
Then deploy the backend and verify every API and worker task is running the
Terraform-produced configuration. Leave read mode, interval allowlist, and
frontend flag unchanged.

Soak for at least one full trading day and through one indexer batch boundary.
Exercise duplicate ingestion and, on Sepolia, a controlled indexer replay.

Pass criteria:

- retries do not increase sample or trade counts;
- out-of-order price observations recompute true OHLC deterministically;
- volume is `ABS(size_delta) * price` before one API-boundary division;
- the Perps cursor and each batch's volume rollups commit atomically;
- zero-trade batches advance the canonical volume coverage watermark;
- finalized corrections increment row revision and dataset generation;
- live appends do not churn the closed-page dataset generation;
- a reorg marks volume coverage incomplete until repair/replay republishes it.
- the basket worker emits `basket_price_watermark_advanced` at most every five
  minutes for both admitted and signed-stale latest polls, with
  `checked_through`, `min_publish_time`, `max_publish_time`, `source`, and
  `watermark_reason`; the volume writer emits
  `perps_volume_writer_heartbeat` even when the indexer is caught up or a
  canonical range has no trades; both writer heartbeats report base-minute
  `coverage_state`, `coverage_finalized_through`, and normalized
  `coverage_lag_seconds` after expected source lateness and bucket alignment;
- neither writer-heartbeat-absence alarm fires, and neither the base-minute
  coverage-lag nor coverage-incomplete alarm fires after coverage has been
  initialized by backfill; these alarms are independent of hourly/daily candle
  boundaries;
- the indexer emits `perps_indexer_progress` with
  `canonical_progress_certified=true` and `indexed_through_timestamp` when a
  contiguous canonical batch advances its cursor.

## Gate 4: newest-first backfill

Start with a bounded recent window so the first useful chart ranges become
available quickly. Unix timestamps are inclusive at `from_timestamp` and
exclusive at `to_timestamp`, and must align to whole minutes.

```bash
run_candle_admin sepolia backfill all \
  -f from_timestamp=FROM_UNIX \
  -f to_timestamp=TO_UNIX
```

Extend toward inception in repeated runs. Coverage must only be published for
contiguous completed chunks. A failed or cancelled task can be rerun with the
same inputs; range replacement and recomputation are idempotent.

After each tranche:

```bash
run_candle_admin sepolia status none
run_candle_admin sepolia verify none \
  -f from_timestamp=FROM_UNIX \
  -f to_timestamp=TO_UNIX
```

Pass criteria for every canonical interval:

- expected and actual non-empty bucket counts match;
- open, high, low, close, sample count, exact volume numerator, trade count,
  and source-block bounds reconcile with canonical source rows;
- price and volume coverage are both complete and overlap the intended range;
- price finalized bounds do not exceed the latest successful basket-worker
  `basket_price_watermark_advanced.checked_through` value, and volume finalized
  bounds do not exceed the latest certified
  `perps_indexer_progress.indexed_through_timestamp` or the timestamp of the
  canonical indexer cursor block. Require
  `canonical_progress_certified=true` for indexer evidence. These are
  independent operational checks against worker/indexer logs and RPC/DB
  cursor evidence; `candle-admin verify` reconciles source rows but cannot
  prove those external progress bounds by itself;
- no partial tranche is exposed as complete coverage.

## Gate 5: deterministic reconciliation and soak

Keep `perps_candle_read_mode = "legacy"`, the public interval allowlist empty,
`perps_candle_shadow_sample_bps = 0`, and the frontend flag off. The v1
correctness gate is the deterministic `plether-candle-admin verify`
reconciliation against canonical PostgreSQL source rows; it does not depend on
sampled request traffic.

Run verification over the intended canary range at the start of the soak:

```bash
run_candle_admin sepolia verify none \
  -f from_timestamp=FROM_UNIX \
  -f to_timestamp=TO_UNIX
```

Continue dual writes for at least one full trading day and through another
Perps indexer batch boundary. Then extend `TO_UNIX` through the newly finalized
range and run the same `verify` command again. Also run `status` and preserve
the two verification run IDs, source bounds, dataset generations, and RDS/API
metrics in the change record.

`PERPS_CANDLE_READ_MODE=shadow` and
`PERPS_CANDLE_SHADOW_SAMPLE_BPS` remain reserved compatibility settings for a
future bounded comparator. They perform no comparison or traffic switch in v1
and must not be used as rollout evidence.

Pass criteria:

- both deterministic verification runs exit successfully for price and volume;
- every canonical interval has matching bucket counts, OHLC/sample values,
  exact volume/trade values, source-block bounds, and current derivation
  metadata;
- coverage remains complete, finalized watermarks advance monotonically, and
  all intervals retain one consistent dataset generation through the soak;
- duplicate ingestion or a controlled Sepolia replay does not change canonical
  counts or values;
- p95/p99 and RDS load stay within the acceptance criteria in ADR 0001.

## Gate 6: backend interval canary

Set `perps_candle_read_mode = "rollup"` and add intervals one at a time to
`perps_candle_read_intervals`. A conservative order is `3600`, `86400`, `900`,
`300`, `1800`, `180`, then `60`. Apply the Terraform change before dispatching
the image-only backend deployment; that workflow deliberately preserves the
environment from the currently registered task definitions.

For each interval, test an inception-clipped page, a fully closed page, the
active page, current candle, a weekend gap, and pagination across at least two
pages. Requests with duplicate, missing, unknown, unaligned, noncanonical, or
far-future query parameters must fail closed and must not be shared-cached.
The server may accept only the immediately adjacent future page to tolerate a
browser/backend clock difference at a page boundary. Component-bearing legacy
history is intentionally restricted to the supported `24h`/`3600` shape;
other component requests must fail with `400` instead of scanning raw history.
The allowed component request must not scan account activity: verify its
per-point `volumeUsdc` is the deliberate non-authoritative zero, its
`plether_db_volume` timing and volume-row count are zero, and market stats remain
the authoritative rolling 24-hour volume source.

Pass criteria:

- rollup SQL p95/p99 is at most 50/100 ms;
- backend p95/p99 is at most 150/300 ms;
- direct origin p95/p99 is at most 750 ms/1 s;
- history pages contain no more than 500 strictly ascending finalized candles;
- clients count actual candles across sparse weekend gaps;
- one browser history traversal stops after at most 24 fixed pages even if a
  chart library supplies an unexpectedly large `countBack`;
- current responses are mutable full replacements, not appended deltas;
- mixed dataset generations force a cache reset and clean pagination restart.

## Gate 7: frontend and edge

Set the environment-specific repository variable
`VITE_PERPS_CANDLE_API_ENABLED_SEPOLIA=true` and deploy the frontend. The
mainnet variable remains false. The worker allowlist normalizes only exact
candle query shapes, uses single-flight origin refreshes, short TTLs for the
active/current candle, and long stale-while-revalidate for closed pages. Before
serving a closed-page cache entry, the worker obtains the authoritative current
series identity from origin and includes `seriesId`, `configurationHash`,
`displayPriceCap`, and `datasetGeneration` in its internal Cache API key. If
that probe is unavailable or incomplete, it bypasses the shared page cache;
if a fetched page does not match the probed identity, it is not cached.

Pass criteria:

- a normal browser chart load uses no more than two history pages plus the
  current endpoint; each edge history request may also perform one bounded
  current-identity origin read before serving a generation-bound cache entry;
- repeated concurrent cache misses produce one origin request per worker
  isolate/cache key;
- a corrected or reorg-invalidated generation cannot serve an older cached
  fork, including to a first-time browser with no local identity baseline;
- raw `2 - price` display conversion swaps high and low correctly;
- the chart advertises the weekday FX session and does not synthesize weekend
  bars;
- edge-hit p95 is at most 150 ms and initial chart history p95 is at most 1 s;
- one component-rich `24h`/hourly legacy request supplies both the headline
  change and component rail; the frontend does not issue a duplicate
  non-component history request and refreshes the component payload no more
  often than every five minutes;
- the 24-hour volume headline comes from market stats, never from the
  component-history compatibility payload.

## Mainnet promotion

The Sepolia self-approval exception is temporary rollout policy, not the
steady-state administration policy. After Gate 7, provision an independent
Sepolia reviewer, merge a follow-up change that restores
`prevent_self_review=true` enforcement for Sepolia, and update
`candle-admin-sepolia` to match. Until that cleanup is complete, record the
exception explicitly in every Sepolia candle-admin change. Never extend the
exception to mainnet.

Repeat every gate for mainnet; do not copy Sepolia coverage or assume Sepolia
capacity measurements apply. Before Gate 2, capture the manual snapshot ID,
available storage, storage type, provisioned IOPS and throughput,
`BurstBalance`, storage-optimization status, estimated growth, and rollback
owner in the change record. Before each gate, verify the deployed `master` SHA
and ensure no duplicate workflow run targets it.

Use smaller backfill chunks or a larger throttle when production RDS has less
headroom. Mainnet read and frontend flags stay off until complete price and
deployment-specific volume coverage has passed verification.

## Rollback

Rollback is flag-based and does not delete data:

1. Set the environment-specific frontend candle flag to `false` and deploy the
   frontend.
2. Set `perps_candle_read_mode = "legacy"` and clear
   `perps_candle_read_intervals`; apply Terraform, then deploy the backend so
   the rollback task definitions and current image are registered together.
3. If rollup writers are implicated, set `perps_candle_write_mode = "off"` and
   apply Terraform, then deploy the API, basket worker, and Perps indexer
   together.
4. Confirm legacy history, latest basket data, and chart rendering recover.
5. Preserve candle tables, coverage state, raw observations, logs, task IDs,
   and the database snapshot for diagnosis.
6. Repair a bounded range only after identifying the source of corruption.
   Repair bounds must be UTC-day aligned so all seven canonical parent
   resolutions can be rebuilt and republished together:

   ```bash
   run_candle_admin sepolia repair all \
     -f from_timestamp=FROM_UNIX \
     -f to_timestamp=TO_UNIX
   ```

7. Run `verify` before re-enabling any interval. Re-enter the rollout at the
   failed gate; never skip directly to the frontend flag.

Rollback is complete only when rollup routes are closed, the frontend is back
on the legacy feed, user-visible checks pass, and error/latency metrics have
returned to baseline.
