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
- Run database mutations and controlled indexer replay only through
  `.github/workflows/candle-admin.yml`
  from `master`. Both `candle-admin-sepolia` and `candle-admin-mainnet` must
  have at least one required reviewer, require an explicit approval, disable
  administrator bypass, and allow only `master`. During the supervised Sepolia
  rollout, its sole administrator may self-approve when no independent reviewer
  is available, so `prevent_self_review` must be false there. Mainnet must still
  require an independent reviewer with `prevent_self_review` set to true.
- Backfill, repair, and controlled replay are accepted only while the relevant
  deployed candle writers are in `PERPS_CANDLE_WRITE_MODE=dual`. Replay also
  requires exactly one Perps indexer topology: consolidated XOR standalone.
  The admin workflow and backend deployment share one environment-specific
  concurrency group so a deployment cannot change write mode or topology
  while an operation is reconciling canonical sources.
- For candle-admin operations, the workflow must resolve the stable API
  service's deployed image digest and preserve its task definition. For replay,
  it must resolve the sole stable Perps indexer topology, pin the unanimous
  running indexer digest, and derive a task containing only that indexer and
  its exact FireLens sidecar; dependencies on excluded containers are rejected.
  For the frozen-finalizer control, it must additionally require the workflow
  SHA to be the exact deployed API/router version, one complete writer topology
  with matching chain/database/candle configuration, and unanimous application
  and FireLens digests before pinning both images. In every case it verifies
  the started task reports the pinned digest and confirms cleanup of the
  workflow-owned temporary revision. A commit-tagged deployed definition is
  not itself sufficient evidence that the one-off task pulled the deployed
  image.
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
- Stop a backfill or replay when an RDS pressure alarm fires, API p95 exceeds
  750 ms for three periods, API 5xx errors increase, or replication/storage
  health is uncertain. A stopped backfill chunk or idempotent replay range is
  safe to retry.
- Each one-off task has an application-enforced absolute runtime and a shorter
  workflow deadline. The workflow assigns a unique ECS `startedBy` identity,
  idempotent client token, and tagged temporary revision; persists the task ARN
  immediately; and uses `always()` cleanup to recover the exact owned task and
  revision after a normal failure or cancellation. Cleanup must confirm the
  task is `STOPPED` before confirming the revision is `INACTIVE`. A hard runner
  loss can prevent cleanup, so the application deadline remains the final
  bound and the next run must reject any nonterminal task or active temporary
  revision until an operator reconciles it.
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
    -f throttle_ms=250 \
    -f confirmation="RUN ${action:u} ON ${environment:u}" \
    "$@"
}
```

The function uses zsh uppercase expansion. With another shell, pass the
workflow fields directly. An omitted `lock_timeout_ms` defaults to `60000` for
`migrate` and `5000` for every other action; use an explicit field only for an
intentional override. After every dispatch, locate and watch the exact run:

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
describe the stable services and their tasks, register and deregister the
dedicated `plether-<environment>-candle-admin` task-definition family, tag and
list tags for its temporary revisions, list that exact family and its tasks,
run and stop tasks, and pass the selected task and execution roles. The
required read/tag actions therefore include `ecs:DescribeServices`,
`ecs:DescribeTasks`, `ecs:DescribeTaskDefinition`, `ecs:ListTasks`,
`ecs:ListTaskDefinitions`, `ecs:ListTagsForResource`, and `ecs:TagResource` in
addition to the scoped register/run/stop/deregister/pass-role permissions. The
finalizer control additionally requires `rds:DescribeDBInstances`,
`rds:DescribePendingMaintenanceActions`, `ec2:DescribeSubnets`,
`ec2:DescribeSecurityGroups`, and `ssm:GetParameter`
scoped to `/plether/sepolia/database-url` where IAM supports resource scoping.
The workflow reads only that parameter's ARN/version/modification metadata and
never emits its decrypted value; after ECS injects the value, the executable
compares its canonical host and database to the validated RDS endpoint and
rejects every ambient libpq `PG*` override before creating a connection or
taking any database lock. The
workflow itself validates that the copied role ARNs are exactly
`plether-<environment>-ecs-task` and
`plether-<environment>-ecs-execution`; this validation constrains the task it
will launch, not the AWS identity's ambient IAM permissions. During the
supervised Sepolia rollout, the existing privileged static deployment
credential remains an explicitly accepted temporary exception. Do not extend
that exception to mainnet: provision a narrowly scoped environment credential
before mainnet candle administration, and migrate both environments to OIDC as
the durable follow-up. Ordinary candle administration copies the API
definition's execution fields and every container, changing only the
`plether-api` image. The finalizer control also pins the API definition's exact
`otel-log-router` FireLens image. Replay copies the selected writer definition's
execution fields but retains only `plether-perps-indexer` and its exact
FireLens sidecar, pinning both to their unanimous running digests. The workflow
never prints environment variables, secret values, or generated definition
JSON.

Before fault injection, also preflight the operator evidence identity. It must
be able to query the Sepolia log group (`logs:StartQuery`,
`logs:GetQueryResults`, `logs:StopQuery`, and the required log-group/stream
describe reads), call `cloudwatch:DescribeAlarms`,
`cloudwatch:DescribeAlarmHistory`, `cloudwatch:GetMetricData`, and
`cloudwatch:GetMetricStatistics`, read the relevant ECS/RDS/SSM metadata, and
resolve the exact ALB dimensions with
`elasticloadbalancing:DescribeLoadBalancers` and
`elasticloadbalancing:DescribeTargetGroups`. Prove these reads succeed before
choosing `B`; discovering missing evidence permissions after the fault fails
the control.

The workflow passes candle administration an application deadline of 19,800
seconds. Replay has a stricter 1,800-second application deadline and a
2,700-second task deadline, leaving fifteen minutes for a clean ECS stop and
task-definition cleanup. The finalizer control has a 2,100-second application
deadline, a 2,400-second task deadline, and a 90-minute job deadline that leaves
bounded reconciliation time after the task deadline. The
executables accept runtime options for local diagnosis, but production
mutations, replay, and the frozen-finalizer control must continue to use the
protected workflow.

## Gate 1: compatible deployment

Merging compatible code to `master` may trigger the repository's automatic
mainnet frontend deployment before Sepolia. Backend deployment is manual for
both environments. This exception is safe only while the candle behavior
remains inert: writes off, legacy reads, an empty interval allowlist, and the
frontend candle flag false. Treat any automatic frontend deployment as code
compatibility only; it does not authorize migration, dual writes, backfill,
rollup reads, or frontend feature activation.

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
PERPS_CANDLE_FINALIZATION_GRACE_SECONDS=15
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

When no override is supplied, `migrate` uses a 60-second PostgreSQL lock
timeout so the concurrent index builders can wait through brief catalog lock
contention without waiting indefinitely. This action builds schema and indexes;
it does not run the candle data backfill. A migration failure must be recorded
as a migration failure, not as a backfill failure.

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

Use an evidence-based Sepolia soak instead of a fixed wall-clock delay. Before
replay, record at least three consecutive healthy five-minute observation
periods after the dual-write deployment reaches steady state. Across that
window require current price and volume writer heartbeats, at least three
certified Perps indexer batch advances, every Sepolia alarm in `OK`, stable ECS
task identities, an available RDS instance with no pending modifications, and
no writer-failure, reorg, API-error, or RDS-pressure signal. Restart the
observation window after any task replacement or failed criterion.

Once that evidence is present, exercise duplicate ingestion by running one
controlled replay over a previously indexed finalized range. Block bounds are
inclusive and may cover at most 5,000 blocks; begin with a materially smaller
range. `scope` must be `none`, both timestamp fields must be blank, and replay
is prohibited outside Sepolia.

```bash
run_candle_admin sepolia replay none \
  -f from_block=FROM_BLOCK \
  -f to_block=TO_BLOCK
```

The protected workflow requires the exact confirmation `RUN REPLAY ON
SEPOLIA`, one stable dual-write indexer topology (consolidated XOR standalone),
and one unanimous running indexer image digest. Its derived ECS task runs only
`plether-perps-indexer` plus the exact FireLens sidecar, with both running image
digests pinned and verified. Replay is one bounded database transaction with a
30-minute application deadline; it neither
advances nor rewinds the canonical cursor, does not advance or certify candle
coverage, and disables external evidence enrichment. The legacy
`plether-perps-indexer --backfill --from ... --to ...` path is prohibited for
rollout operations and must not be invoked manually.

Pass criteria:

- retries do not increase sample or trade counts;
- out-of-order price observations recompute true OHLC deterministically;
- volume is `ABS(size_delta) * price` before one API-boundary division;
- the Perps cursor and each batch's volume rollups commit atomically;
- zero-trade batches advance the canonical volume coverage watermark;
- finalized corrections increment row revision and dataset generation;
- live appends do not churn the closed-page dataset generation;
- a reorg marks volume coverage incomplete until canonical recovery or repair
  republishes it; controlled replay alone never certifies coverage;
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
  contiguous canonical batch advances its cursor;
- the replay emits `perps_indexer_replay_started` and
  `perps_indexer_replay_complete` (and
  `perps_indexer_replay_failed` on failure), with the exact inclusive block
  bounds and `canonical_progress_certified=false`; preserve its task ARN and
  pinned image digest in the change record, while the canonical indexer cursor
  and coverage certification remain unchanged;
- repeating the same replay range leaves canonical history, rollup counts, and
  values unchanged.

## Gate 4: newest-first backfill

Start with a bounded recent window so the first useful chart ranges become
available quickly. Unix timestamps are inclusive at `from_timestamp` and
exclusive at `to_timestamp`, and must align to whole minutes. The first tranche
must contain at least one fully aligned bucket for every canonical interval.
Because `86400` is canonical and aligns to UTC midnight, use a range that spans
at least one complete UTC day; an arbitrary trailing 24-hour window is not
sufficient.

```bash
run_candle_admin sepolia backfill all \
  -f from_timestamp=FROM_UNIX \
  -f to_timestamp=TO_UNIX
```

Extend toward inception in repeated runs. Coverage must only be published for
contiguous completed chunks. A failed or cancelled task can be rerun with the
same inputs; range replacement and recomputation are idempotent.

Immediately before dispatch, derive the price upper bound from the latest
successful `basket_price_watermark_advanced.checked_through` heartbeat and the
volume upper bound from a certified
`perps_indexer_progress.indexed_through_timestamp`. The price tranche must
finish close enough to the live writer watermark that the first post-backfill
poll remains within `max(300, 2 * PERPS_CANDLE_LATENESS_SECONDS)`; otherwise the
writer correctly invalidates the newly published coverage with
`price_watermark_gap`. Use separate price and volume runs when their safe upper
bounds differ. Re-read both bounds after the run and treat the first live
heartbeat as part of publication success, not as optional soak evidence.

After each tranche:

```bash
run_candle_admin sepolia status none
run_candle_admin sepolia verify all \
  -f from_timestamp=FROM_UNIX \
  -f to_timestamp=TO_UNIX
```

Pass criteria for every canonical interval:

- the interval has a non-empty aligned verification range and an explicit
  complete coverage row; an empty aligned range is not a successful check;
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
- the first live price heartbeat after publication still reports
  `coverage_state=complete`, an empty `coverage_error`, and an acceptable lag;
- no partial tranche is exposed as complete coverage.

### Recover a migration-only closed-market price gap

Do not use generic candle repair when strict coverage is disabled only with
`price_watermark_gap`: there may be no missing candle observation to rebuild.
For a supervised Sepolia migration gap wholly inside the weekly oracle-frozen
window, first read the minute `coverage_end` from `status`. Then run:

```bash
run_candle_admin sepolia recover-closed-price-gap none \
  -f from_timestamp=EXACT_MINUTE_COVERAGE_END \
  -f to_timestamp=SUNDAY_2100_UTC_EXCLUSIVE
```

The protected action dynamically advances through a freshly fetched latest
payload, not blindly through `to_timestamp`. It aborts unless all six
authenticated Pyth Pro histories contain no updates in the gap, the latest payload
passes the deployed onchain Pyth parser, its signed basket exactly matches the
last stored priority-100 observation, all seven coverage rows retain one exact
`price_watermark_gap` generation, and the full approved range stays within
Friday 22:00–Sunday 21:00 UTC. It then republishes coverage atomically with one
new generation and does not insert, delete, or modify candle prices.

Immediately require a normal `basket_price_watermark_advanced` heartbeat with
`coverage_state=complete`, then smoke-check the current and history endpoints.
Any history activity, price mismatch, coverage-state mismatch, approval delay,
or live-session boundary crossing is a hard failure and requires diagnosis;
never widen or bypass the proof.

History evidence must use `PYTH_HISTORY_URL=https://pyth.dourolabs.app/v1`.
The legacy Benchmarks `/v1/shims/tradingview/*` endpoints were retired during
the August 2026 Pyth Core upgrade and a 404 from that host is not evidence of
an empty market interval.

### Operator-selected price-history target

After the target/progress migration and target-capable basket worker are
deployed, select any non-negative Unix timestamp as the public price-history
start through the protected admin workflow:

```bash
run_candle_admin sepolia set-history-target none \
  -f history_start_timestamp=PRICE_TARGET_FROM_UNIX
```

Selection records desired state only. It must not change the currently
published target, public coverage, or dataset generation. The deployed basket
worker freezes a source-safe exclusive end and checks all six Pyth component
histories in bounded two-day windows. A recent or future target intentionally
remains uninitialized until that end contains one full aligned UTC day and at
least one canonical database price source; this prevents an immutable target
from completing in a range that cannot be published after a weekend or market
closure. `status` reports both the desired and
active published revisions, the active generation, frozen bounds, durable next
timestamp, last source error, and whether the exact desired revision is ready
for publication:

```bash
run_candle_admin sepolia status none
```

Do not infer a large empty interval from a nearby no-data response. The
resolution-one Pyth endpoint limits the requested range before checking whether
it contains data, so progress through very old empty history is intentionally
proportional to the selected range. Unequal component update timestamps are
normal; the worker uses per-feed as-of closes only within a five-minute
freshness bound and records an empty proved window when no complete fresh
basket exists. If trusted price coverage already exists, overlap is proved but
not rewritten; only samples before its published start are persisted for the
later protected publication and rebuild.

When `publication_ready=true`, publish price only. Omit bounds so CandleAdmin
uses the exact frozen ingestion proof (supplying `--from` or `--to` is accepted
only when both match it exactly):

```bash
run_candle_admin sepolia backfill price
run_candle_admin sepolia status none
run_candle_admin sepolia verify price \
  -f from_timestamp=PRICE_TARGET_FROM_UNIX \
  -f to_timestamp=FROZEN_PRICE_END_UNIX
```

The protected backfill builds unpublished price chunks, then rechecks that the
target is still latest and its proof is complete before publishing coverage,
advancing the price generation, and activating the target in one transaction.
A cancellation, source error, or replacement target leaves the prior published
history readable. Require at least one canonical price sample and one full
aligned bucket for every canonical interval before publication.

This operation does not backfill volume from old contracts. Native candles
before the currently configured router's proven volume coverage must contain
`volumeUsdc: null`, `longFlowVolumeUsdc: null`,
`shortFlowVolumeUsdc: null`, `tradeCount: null`, and
`volumeComplete: false`. Within that current-router coverage, a missing volume
row is a proven zero. Never run cross-release ingestion or substitute zero for
unknown pre-router volume.

## Gate 5: deterministic reconciliation and soak

Keep `perps_candle_read_mode = "legacy"`, the public interval allowlist empty,
`perps_candle_shadow_sample_bps = 0`, and the frontend flag off. The v1
correctness gate is the deterministic `plether-candle-admin verify`
reconciliation against canonical PostgreSQL source rows; it does not depend on
sampled request traffic.

Run verification over the intended canary range at the start of the soak. For
`verify all`, `FROM_UNIX..TO_UNIX` must be inside the common overlap of price
coverage and the currently configured `(chain_id, router)` volume coverage.
It is deliberately independent of `PRICE_TARGET_FROM_UNIX`: any earlier price
prefix is certified by the `verify price` command above and does not require
old-router volume.

```bash
run_candle_admin sepolia verify all \
  -f from_timestamp=FROM_UNIX \
  -f to_timestamp=TO_UNIX
```

Continue dual writes through three consecutive healthy five-minute observation
periods and at least three additional certified Perps indexer batch advances.
Require the same stable-service, writer-heartbeat, alarm, RDS, and API evidence
as Gate 3, restarting the observation window after any failed criterion or task
replacement. Then extend `TO_UNIX` through the newly finalized range and run
the same `verify` command again. Also run `status` and preserve the two
verification run IDs, source bounds, dataset generations, and RDS/API metrics
in the change record.

Repeat the Gate 3 protected replay once over the same bounded finalized block
range, or over another explicitly recorded range of at most 5,000 inclusive
blocks, and rerun `verify`. Do not replace it with the legacy indexer
`--backfill` mode. Preserve the replay workflow run ID, ECS task ARN, selected
writer topology, pinned image digest, bounds, and structured completion event.

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
- duplicate ingestion or the protected Sepolia replay does not change
  canonical counts or values, advance/rewind the canonical cursor, or certify
  coverage;
- p95/p99 and RDS load stay within the acceptance criteria in ADR 0001.

## Gate 6: backend interval canary

Set `perps_candle_read_mode = "rollup"` and, by default, add intervals one at a
time to `perps_candle_read_intervals`. A conservative order is `3600`, `86400`,
`900`, `300`, `1800`, `180`, then `60`. Apply the Terraform change before
dispatching the image-only backend deployment; that workflow deliberately
preserves the environment from the currently registered task definitions.

After the Sepolia `3600` canary completes its frozen-finalizer control, the
supervised rollout may batch-enable the remaining canonical intervals in one
change only when all of the following are recorded explicitly:

- deterministic price and volume verification already passed for all seven
  intervals;
- the hourly load run had zero request failures and passed the public HTTP
  latency targets; and
- the operator explicitly accepts any missed internal application or database
  sub-budget as a follow-up rather than representing the unchanged gate as a
  full pass.

Use `infra/terraform/sepolia-candle-rollout.tfvars` for that final Sepolia
expansion. A batch does not waive interval correctness: after the resulting
task replacement, collect a fresh three-period health window and run the full
current, active, closed, inception, weekend-gap, pagination, invalid-request,
cache, and latency matrix for every enabled interval. Keep the frontend flag
off until the entire matrix passes. On any failure, restore the last accepted
allowlist. This batching exception does not apply to mainnet.

For each interval, test an inception-clipped page, a fully closed page, the
active page, current candle, a weekend gap, and pagination across at least two
pages. Requests with duplicate, missing, unknown, unaligned, noncanonical, or
far-future query parameters must fail closed and must not be shared-cached.
Run the active-page and current-candle probes across a real
`bucket end + PERPS_CANDLE_LATENESS_SECONDS` boundary. The previous stored
watermark may remain readable only during
`PERPS_CANDLE_FINALIZATION_GRACE_SECONDS`; require zero 5xx responses while the
writer publishes, and verify that a deliberately frozen finalizer fails closed
when the grace expires.

The legacy basket-history route remains an availability/rollback smoke only
until its component/headline consumers migrate and a separate follow-up
removes it. Do not include it in Gate 6 rollover, latency, or performance
acceptance evidence, and do not optimize it as part of this rollout.

Run the frozen-finalizer control once, only for the Sepolia `3600` canary. Keep
the frontend flag off and stop all native candle-page traffic and probes,
including requests whose edge cache lookup performs an internal current-candle
identity probe. Exactly the three operator requests below may reach the current
endpoint during the bounded control window. Freeze Terraform, SSM rotations,
manual ECS/service changes, and backend/frontend deployments from preflight
until all post-control evidence and cleanup are complete. Require the merged
`master` SHA to be the exact version already deployed to the stable API and
writers, with identical pre/post source definitions and running task sets.

Before choosing the boundary, require every Sepolia alarm `OK`, RDS `available`
with no pending changes, and no preferred backup or maintenance window that
overlaps the candidate `[B + 45, B + 300)` evidence interval. Require no
pending RDS action whose `CurrentApplyDate`, `AutoAppliedAfterDate`, or
`ForcedApplyDate` is at or before `B + 300`; preserve the initial and
immediate-pre-`RunTask` workflow snapshots because the maintenance API is
eventually consistent. Also require a current complete/error-free
`basket_price_watermark_advanced` heartbeat, a current complete/error-free
`perps_volume_writer_heartbeat` with acceptable normalized lag, at least three
recent `perps_indexer_progress` records with
`canonical_progress_certified=true`, and zero
`basket_price_writer_heartbeat_failed`, `perps_volume_writer_heartbeat_failed`,
or `perps_indexer_iteration_failed` events across that certifying window.
Choose an aligned hour `B` so `B + 105` is between five and 30 minutes away.
Establish an evidence window beginning no later than `B + 45`: require zero
`perps_candle_coverage_unhealthy` events and zero unthrottled API 5xx responses
on every route continuously through `B + 139`. Then dispatch only from merged
`master`:

```bash
gh workflow run candle-admin.yml \
  --repo Plether-Fi/plether-app \
  --ref master \
  -f environment=sepolia \
  -f action=finalizer-probe \
  -f scope=none \
  -f boundary="$B" \
  -f 'confirmation=RUN FINALIZER-PROBE ON SEPOLIA'
```

Immediately before dispatch, preserve the remote `master` SHA from
`gh api repos/Plether-Fi/plether-app/commits/master --jq .sha`. Locate the one
new workflow-dispatch run with `gh run list`, then preserve and inspect:

```bash
gh run view RUN_ID \
  --repo Plether-Fi/plether-app \
  --json event,headBranch,headSha,status,conclusion,url
```

Require `event=workflow_dispatch`, `headBranch=master`, and `headSha` equal to
the pre-recorded remote SHA and the deployed API/router `SERVICE_VERSION`;
otherwise cancel the control before the timed requests.

The protected workflow rejects a nonempty temporary task family, validates one
complete stable writer topology, matches the API and writers on the exact
chain, router, SSM parameter ARN/version, RDS endpoint/resource/VPC,
pending-maintenance horizon, candle configuration, application digest,
FireLens digest, and deployed workflow SHA, and pins both task images. It
proves every running API/writer task was created
no earlier than the selected parameter modification and every one-off subnet and
security group belongs to that RDS VPC. The executable validates the resolved
`DATABASE_URL` host and database before creating a connection, takes the global
candle-admin lock, then at `B + 105` takes the price dataset's
transaction-scoped writer lock inside a read-committed, read-only transaction.
It requires the price hourly watermark to be exactly `B - 3600`, the volume
hourly watermark to have reached `B`, and `dxy-v1` to remain active throughout
the control window. It performs no row mutation and never stops or scales an
ECS service. Database-clock polling is at most five seconds apart and a
12-second idle-transaction timeout independently releases a wedged connection.

The current route samples its backend wall clock once and uses that exact
integer Unix second for database selection and strict freshness validation. It
must return the same value in exactly one
`X-Plether-Candle-Validated-At` response header on both success and error
paths. Use that origin-specific header as the API-clock evidence; do not infer
the backend clock from the standard `Date` header, which is whole-second,
generated at an unspecified response stage, and may be replaced by an edge
intermediary. Keep database lifecycle timestamps as separate evidence for the
probe's lock, grace, release, and recovery schedule; do not combine them with
an edge clock into a synthetic offset bound. The literal no-retry request and
two-second timeout remain mandatory.
Require exactly one lifecycle scoped to the recorded task ARN/log stream and
workflow run: `perps_candle_finalizer_probe_scheduled` ->
`perps_candle_finalizer_probe_lock_acquired` (database field `acquired_at`
between `B + 105` and `B + 110`) ->
`perps_candle_finalizer_probe_grace_expired` (`observed_at >= B + 135`) ->
`perps_candle_finalizer_probe_lock_released` (`released_at` between `B + 150`
and `B + 155`) -> `perps_candle_finalizer_probe_recovery_complete`
(`recovered_at <= B + 165`). Require no
`perps_candle_finalizer_probe_failed` from that task; that failure event has no
boundary field, so task/run scope is mandatory. Abort on a missing, duplicate,
out-of-order, or wrong-boundary lifecycle event.

Do not send the first request unless the exact acquired event is available by
`B + 125`; if it is available, send once at `B + 130` and require `200`. CloudWatch/FireLens
delivery has no five-second SLA, so do not delay the fault request waiting for
the asynchronously delivered grace-expired log. Instead, schedule it from the
verified clocks at `B + 140`, after confirming by `B + 138` that the exact ECS
target container remains `RUNNING`, and require `503`; afterward, verify the
grace-expired event's database timestamp and lifecycle order. Both requests use
this exact URL:

```text
https://app.sepolia.plether.com/api/perps/v1/perps/basket/candles/current?interval=3600
```

For each of `before-grace`, `fault`, and `after-recovery`, set `ARTIFACT` to a
different evidence-path prefix and run this literal no-retry request. Do not add
a query parameter or change the URL:

```bash
date -u +%Y-%m-%dT%H:%M:%S.%NZ > "${ARTIFACT}.started-at"
curl --disable --silent --show-error --retry 0 --max-time 2 \
  --request GET \
  --header 'Cache-Control: no-store' \
  --header 'Pragma: no-cache' \
  --header 'If-None-Match:' \
  --header 'If-Modified-Since:' \
  --dump-header "${ARTIFACT}.headers" \
  --output "${ARTIFACT}.body" \
  --write-out '%{http_code} %{time_starttransfer} %{time_total}\n' \
  'https://app.sepolia.plether.com/api/perps/v1/perps/basket/candles/current?interval=3600' \
  > "${ARTIFACT}.timing"
date -u +%Y-%m-%dT%H:%M:%S.%NZ > "${ARTIFACT}.finished-at"
```

Preserve every response header, including `Date`,
`X-Plether-Candle-Validated-At`, `Cache-Control`, `CF-Cache-Status`,
`Server-Timing`, and any `X-Plether-Edge-Cache`. Require exactly one canonical
integer candle-validation header on each response. The `before-grace` value
must be less than `B + 135`; the intentional `fault` value must be at least
`B + 135` and less than `B + 150`; and the `after-recovery` value must not be
less than the fault value. These are the exact clock values used to produce
the three outcomes, so do not replace them with a midpoint estimate from
`Date`. Also require
`X-Plether-Edge-Cache` to be absent, `Server-Timing` to contain
`plether_edge_origin` and no edge-cache timing, and `CF-Cache-Status` to be
absent, `DYNAMIC`, or `BYPASS`. Reject `HIT`, `STALE`, `REVALIDATED`, `UPDATING`,
`MISS`, `EXPIRED`, validators, an unbounded request, or any retry because those
do not prove a direct origin observation. The task releases at `B + 150` and
fails if release is later than `B + 155`. It samples recovery in one fresh
repeatable-read snapshot: price must advance from `B - 3600` to at least `B`,
volume must remain healthy at or beyond `B`, and both generations must remain
unchanged by `B + 165`. Do not issue the third request until the matching
`recovery_complete` event exists; then issue it once with the same command and
require `200`.

Validate both `200` bodies directly; statuses and headers alone are
insufficient. Set `BEFORE_GRACE_ARTIFACT` and `AFTER_RECOVERY_ARTIFACT` to the
two saved prefixes. Set `PRICE_GENERATION` and `VOLUME_GENERATION` to the exact
positive integers from the scoped lock-acquired event. Set `VOLUME_ROUTER` to
the exact lower-case `PERPS_ORDER_ROUTER` value from the pinned task
definition, first confirm the recovery-complete event contains the same
generation pair, and run:

```bash
jq --exit-status --null-input \
  --slurpfile before "${BEFORE_GRACE_ARTIFACT}.body" \
  --slurpfile after "${AFTER_RECOVERY_ARTIFACT}.body" \
  --argjson boundary "$B" \
  --argjson price_generation "$PRICE_GENERATION" \
  --argjson volume_generation "$VOLUME_GENERATION" \
  --arg volume_router "$VOLUME_ROUTER" '
    def integer: type == "number" and floor == .;
    def optional_nonnegative_integer:
      . == null or (integer and . >= 0);
    def candle_schema($expected_timestamp):
      . == null or
      (type == "object"
       and keys == ["complete", "longFlowVolumeUsdc", "priceComplete", "quality",
                    "rawClosePrice", "rawHighPrice", "rawLowPrice", "rawOpenPrice",
                    "revision", "sampleCount", "shortFlowVolumeUsdc", "timestamp",
                    "tradeCount", "volumeComplete", "volumeUsdc"]
       and (.timestamp | integer) and .timestamp == $expected_timestamp
       and ([.rawOpenPrice, .rawHighPrice, .rawLowPrice, .rawClosePrice]
            | all(type == "string" and test("^[1-9][0-9]*$")))
       and (.volumeUsdc == null
            or (.volumeUsdc
                | type == "string" and test("^[0-9]+$")))
       and ((.longFlowVolumeUsdc == null and .shortFlowVolumeUsdc == null)
            or ([.longFlowVolumeUsdc, .shortFlowVolumeUsdc]
                | all(type == "string" and test("^[0-9]+$"))))
       and (.tradeCount | optional_nonnegative_integer)
       and (.sampleCount | integer) and .sampleCount >= 0
       and (.revision | integer) and .revision >= 0
       and (.quality == "observed"
            or .quality == "legacy_sampled"
            or .quality == "mixed")
       and .priceComplete == false
       and .volumeComplete == false
       and .complete == false);
    def current_schema($expected_timestamp):
      type == "object"
      and keys == ["candle", "configurationHash", "coverageComplete",
                   "coverageEnd", "coverageStart", "datasetGeneration",
                   "displayPriceCap", "finalizedThrough", "intervalSeconds",
                   "seriesId", "volumeChainId", "volumeCoverageComplete",
                   "volumeCoverageEnd", "volumeCoverageStart",
                   "volumeFinalizedThrough", "volumeRouter"]
      and .intervalSeconds == 3600
      and .seriesId == "dxy-v1"
      and (.configurationHash
           | type == "string" and test("^sha256:[0-9a-f]{64}$"))
      and (.displayPriceCap
           | type == "string" and test("^[1-9][0-9]*$"))
      and .volumeChainId == 421614
      and .volumeRouter == $volume_router
      and .volumeCoverageComplete == true
      and (.volumeCoverageStart | integer)
      and (.volumeCoverageEnd | integer)
      and .volumeCoverageStart < .volumeCoverageEnd
      and (.volumeFinalizedThrough | integer)
      and .volumeFinalizedThrough >= .volumeCoverageStart
      and .volumeFinalizedThrough <= .volumeCoverageEnd
      and (.datasetGeneration | integer) and .datasetGeneration > 0
      and .coverageComplete == true
      and (.coverageStart | integer)
      and (.coverageEnd | integer)
      and .coverageStart < .coverageEnd
      and (.finalizedThrough | integer)
      and (.candle | candle_schema($expected_timestamp));
    def envelope_schema($expected_timestamp):
      type == "object"
      and keys == ["data", "meta"]
      and (.meta | type == "object")
      and (.meta | keys == ["blockNumber", "cached", "chainId"])
      and .meta.cached == false
      and .meta.blockNumber == 0
      and .meta.chainId == 421614
      and (.data | current_schema($expected_timestamp));
    def identity:
      {seriesId, configurationHash, displayPriceCap, volumeChainId,
       volumeRouter, datasetGeneration};
    ($price_generation | integer and . > 0 and . < 67108864)
    and ($volume_generation | integer and . > 0 and . < 67108864)
    and (($price_generation * 134217728 + $volume_generation * 2 + 1) as $generation
         | ($before | length) == 1
           and ($after | length) == 1
           and ($before[0] | envelope_schema($boundary))
           and ($after[0] | envelope_schema($boundary))
           and $before[0].data.finalizedThrough == ($boundary - 3600)
           and $after[0].data.finalizedThrough >= $boundary
           and ($before[0].data | identity) == ($after[0].data | identity)
           and $before[0].data.datasetGeneration == $generation
           and $after[0].data.datasetGeneration == $generation)
  '
```

This parses both bodies, enforces the current-candle schema and hourly
`dxy-v1` identity, and binds both successful observations to the probe's
unchanged database generations. The public generation reserves the low bit
for usable volume; this healthy-volume probe therefore requires that bit to be
set. Preserve both JSON bodies as evidence.

The intentional `503` must produce exactly one
`perps_candle_coverage_unhealthy` event with `request_kind=current`,
`interval_seconds=3600`, reason `finalized watermark is stale after the
configured publication grace`, and `suppressed_count` absent or zero. Prove the
same single failure independently with exactly one unthrottled
`api_foreground_request_completed` event for
`http_route=/api/perps/basket/candles/current` and `http_status_code=503`, and
exactly three total unthrottled completion events for that route, correlated to
the three request artifacts with ordered statuses `200`, `503`, `200`. Require
no additional current-candle request event and no other unthrottled API 5xx in
the control window. Require the `PerpsCandleCoverageUnhealthy-sepolia` metric's
exact aligned `[B + 120, B + 180)` 60-second `Sum` to be one; in
`[B + 180, B + 240)`, require no positive datapoint (the metric normally has no
point because its log filter has no default value). Separately wait through
`B + 300` plus CloudWatch ingestion, then require the Application ELB
`HTTPCode_Target_5XX_Count` exact LoadBalancer/TargetGroup 300-second `Sum` for
`[B, B + 300)` to be one and prove zero other target 5xx across that whole
period. Only
`plether-sepolia-candle-coverage-unhealthy` may transition from `OK` to `ALARM`;
observing that transition is required evidence, not a waiver. Require that
alarm to return to `OK` after a full clean aligned 60-second evaluation period
and every other alarm to remain `OK`. Preserve `describe-alarm-history` for the
named alarm and every other Sepolia alarm over the full evidence window;
correlate the named transition to `[B + 120, B + 180)`, and use `GetMetricData`
or `GetMetricStatistics` to preserve the exact unhealthy point, following
no-positive-point period, and exact ALB 300-second point. Do not use `set-alarm-state`,
change thresholds, disable actions, or clear metric data.

Preserve one change record for the entire certifying window: `master`/head SHA,
workflow run ID and URL, boundary, selected topology, source and temporary task
definitions, task ARN, application and FireLens digests, scheduled/acquired/
grace-expired/released/recovery log events and absence of a failure event,
pre/post generations and watermarks, all three HTTP timestamp/header/status/
body artifacts, exact foreground/unhealthy events and metric points, complete
state history for every alarm, exact pre/post source definitions and task sets,
RDS endpoint/resource/VPC and dual pending-maintenance snapshots, exact SSM
parameter ARN/version evidence, the named current writer
heartbeats, three certified indexer advances, zero named writer failures, and
confirmed task `STOPPED` and temporary definition `INACTIVE` cleanup. Workflow
and task success prove only the database lock/recovery lifecycle; Gate 6 passes
only after the operator's HTTP, log, metric, alarm, health, and cleanup record
is complete. Retrieve workflow evidence with the CLI, never the Actions web UI,
and privately preserve the authoritative final ECS states:

```bash
AWS_REGION=us-east-1
gh run view RUN_ID \
  --repo Plether-Fi/plether-app \
  --log > gate6-finalizer-workflow.log
aws ecs describe-tasks \
  --profile plether \
  --region "$AWS_REGION" \
  --cluster plether-sepolia \
  --tasks "$TASK_ARN" > gate6-finalizer-task.json
aws ecs describe-task-definition \
  --profile plether \
  --region "$AWS_REGION" \
  --task-definition "$TEMPORARY_TASK_DEFINITION" \
  > gate6-finalizer-task-definition.json
jq --exit-status '.tasks | length == 1 and .tasks[0].lastStatus == "STOPPED"' \
  gate6-finalizer-task.json
jq --exit-status '.taskDefinition.status == "INACTIVE"' \
  gate6-finalizer-task-definition.json
```

Any early or additional `503`,
missing/extra event, metric mismatch, missing named-alarm transition, unrelated
alarm transition, task or cleanup failure, generation change, failed
watermark/endpoint recovery, or missing final `OK` fails Gate 6 and triggers
the flag-based rollback. The normal mutation freeze ends only for controlled
failure recovery: first preserve the immutable evidence, then stop/reconcile
the exact workflow-owned task (or wait for its bounded application deadline),
prove its database lock is released, and only then permit the minimum manual
ECS reconciliation or flag/deployment rollback. Never leave a failed cleanup
blocked behind the freeze, and never broaden that exception beyond recovery of
this exact Sepolia control.

The server may accept only the immediately adjacent future page to tolerate a
browser/backend clock difference at a page boundary. While the frontend still
uses component-bearing legacy history before cutover, keep it restricted to
the supported `24h`/`3600` shape; other component requests must fail with `400`
instead of scanning raw history. The allowed request must not scan account
activity: its per-point `volumeUsdc` is the deliberate non-authoritative zero,
its `plether_db_volume` timing and volume-row count are zero, and market stats
remain the authoritative rolling 24-hour volume source. This is transitional
availability/correctness evidence only, not Gate 6 latency evidence.

Collect native candle latency samples using the ADR 0001 protocol. Every
interval and each successful native candle response shape listed above must
meet its applicable thresholds independently. Rejected-input cases remain
correctness checks and are not pooled into latency series. The
single frozen-finalizer `503` is a correctness fault-injection observation: do
not pool, discard, retry, or replace it in latency evidence.

Pass criteria:

- rollup SQL p95/p99 is at most 50/100 ms;
- backend p95/p99 is at most 150/300 ms;
- direct origin p95/p99 is at most 750 ms/1 s;
- no rollover-boundary request fails while a healthy writer publishes within
  the configured finalization grace;
- the supervised frozen-finalizer control produces exactly one expected `503`
  and the matching unthrottled request event, unhealthy event, one-point alarm
  metric, and exact `[B, B + 300)` ALB target-5xx sum; the named alarm alone transitions `OK` to
  `ALARM` to `OK`; writer publication recovers without a generation change;
  cleanup is confirmed; and all alarms are `OK` before further rollout work;
- history pages contain no more than 500 strictly ascending finalized candles;
- clients count actual candles across sparse weekend gaps;
- one browser history traversal stops after at most 24 fixed pages even if a
  chart library supplies an unexpectedly large `countBack`;
- current responses are mutable full replacements, not appended deltas;
- mixed dataset generations force a cache reset and clean pagination restart.

## Gate 7: frontend and edge

Set the environment-specific repository variable
`VITE_PERPS_CANDLE_API_ENABLED_SEPOLIA=true` and deploy the frontend. The
mainnet variable remains false. Do not enable that variable or redeploy an
already-enabled Sepolia native-candle frontend until the current router's
volume has been backfilled and verified. The frontend deployment workflow
queries the direct backend current-candle endpoint for all seven canonical
intervals before any Cloudflare operation. When the Sepolia flag is enabled,
the workflow fails unless every response has complete, aligned price and
volume coverage; a positive price and volume generation; the usable-volume
generation bit; and the same chain/router as the checked-in Sepolia manifest.
Each transient request gets at most three attempts with a ten-second timeout.

For the router deployed at block `302257125`, recovery starts at the first
whole minute after the block timestamp. Never broaden this range to an earlier
router or fill unknown historical volume with zero:

```bash
VOLUME_FROM_UNIX=1787759880

# Read VOLUME_TO_UNIX from the greatest whole-minute timestamp at or below the
# latest perps_indexer_progress.indexed_through_timestamp whose
# canonical_progress_certified field is true.
run_candle_admin sepolia status none
run_candle_admin sepolia backfill volume \
  -f from_timestamp="$VOLUME_FROM_UNIX" \
  -f to_timestamp="$VOLUME_TO_UNIX"
run_candle_admin sepolia status none
run_candle_admin sepolia verify volume \
  -f from_timestamp="$VOLUME_FROM_UNIX" \
  -f to_timestamp="$VOLUME_TO_UNIX"
```

The standard helper supplies 86,400-second chunks, a 1,800,000 ms statement
timeout, and a 250 ms inter-chunk throttle. Record the certified indexer event,
exact exclusive upper bound, workflow run IDs, current router, writer state,
price and volume generations, and verification output. Require all seven
public current-candle routes to show complete volume coverage, the legacy
compatibility history route to return `200`, and both incomplete and
uninitialized coverage alarms to be `OK` before dispatching the frontend from
the verified `master` commit.

The worker allowlist normalizes only exact
candle query shapes, uses single-flight origin refreshes, short TTLs for the
active/current candle, and long stale-while-revalidate for closed pages. Before
serving a closed-page cache entry, the worker obtains the authoritative current
series identity from origin and includes `seriesId`, `configurationHash`,
`displayPriceCap`, `volumeChainId`, `volumeRouter`, and `datasetGeneration` in
its internal Cache API key. The probe also supplies price and scoped-volume
coverage bounds. A page receives the long TTL only when price is finalized
through its full boundary and its volume is either wholly before the scoped
coverage start or finalized through that boundary; terminal and partially
covered pages retain the active TTL. Coverage boundaries are part of the
internal page-state key, so normal watermark advances do not churn immutable
pages while a backwards coverage extension cannot reuse stale null volume. If
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
