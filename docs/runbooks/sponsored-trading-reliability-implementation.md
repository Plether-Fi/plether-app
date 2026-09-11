# Sponsored trading reliability — implementation status

Status: partial implementation, not release-qualified. No deployment or funding
change is authorized by this document. Core v1.2.3, the 60-second order window,
KMS, budget authority, canary restrictions and safe confirmation remain unchanged.

## Implemented locally

- Authenticated, non-cacheable `GET /api/perps/v1/readiness` through the existing
  proxy. Shared backend observations refresh on a ten-second cadence and expire
  after fifteen seconds. The response contains categories and timestamps, not
  addresses, exact balances, provider errors or credentials.
- Sponsorship enablement, onchain paymaster pause, reconciliation freshness,
  Alto EntryPoint availability, keeper heartbeat/funding evidence and the release
  lens's trading-active signal. Inactive open execution never automatically
  blocks closes: exit-policy availability is reported as unknown.
- One shared visible-UI readiness poll, Trading status in account activity
  (removed from trade preview at the operator's request), and an explicit refresh action. Preparation starts the
  read concurrently. Only a fresh blocked result with enforcement enabled stops
  signing; unknown, expired, failed and slow observations do not.
- Idle keeper funding no longer requires a recent transaction quote. Every ten
  seconds the background observer reads a canonical, fixed-block balance/nonce,
  pending nonce, current gas fees and the oracle update fee for the cached payload
  shape. Independent reads run concurrently, outside database connections. Its
  conservative reserve uses the existing 30M gas cap, configured fee buffer and
  maximum batch size; any fresh actual quote can only raise that reserve. The
  cached payload is not evidence of executable prices. Oracle readiness remains
  separate. Missing/malformed evidence, a changed block, a stale/future header or
  unresolved pending liabilities produce unknown. Below ten reserves warns;
  below one conservative reserve is unknown, not a proven insufficient-funding
  blocker. A verified empty account is blocked. No transaction is signed or sent,
  and no RPC is added to the browser's preparation critical path. This follow-up
  is local only until a separately approved keeper deployment.
- Twenty seconds of reviewed order/sponsorship headroom before signing and ten
  seconds before first submission. A late signature is durably journaled, not
  submitted or silently renewed; recovery retains the lane until safe resolution.
- In-flight receipt-read coalescing per runtime/hash. Active polling starts at
  two seconds and moves to five seconds after one minute. Existing cross-tab
  coordination, safe recovery and partial-deposit journaling remain in place.
- Random attempt references in typed frontend analytics, native preparation
  request metadata and durable preparation linkage. A bounded background queue
  and a restart-recovery scan materialize client-bound historical diagnostics.
  Safely reconciled UserOperation outcomes can be recorded after browser closure.
- Restart-safe background execution-attempt correlation uses finalized AA
  evidence and checks the exact release registration within its EntryPoint
  operation's log interval. It verifies the receipt and canonical block hash;
  ambiguous bundles remain unlinked. Short database leases fence multiple API
  instances and recover abandoned work without holding a connection across RPC.
  The keeper indexing/execution path no longer performs this extra receipt RPC.
  Durable keeper errors can be recovered after diagnostic materialization, with
  their original observation timestamps. Historical reasons are never inferred
  from current readiness. The UI reports when a verified cause is unavailable.
- A separate PostHog log projection allowlists categories, counters, random
  references and service resources; raw CloudWatch operational fields remain on
  their original copy. Malformed projection input fails closed. Console capture,
  consent and replay settings are unchanged. Exporter exceptions cannot escape
  frontend capture helpers into signing/recovery.
- Local Frankfurt analytics receive `deployment_name=sepolia-aa-temp`.
  Existing production dashboards were not modified. PostHog project 208816 has
  no existing Frankfurt dashboard; new dashboard/data qualification is pending.
- Readiness telemetry emits each distinct component/reason immediately, deduplicates
  evidence shared across actions and summarizes repeated observations every sixty
  seconds while polling is active. Recovery flushes unreported counts. Unknown or
  missing evidence never declares a dependency recovered. Exported labels use
  exact allowlists; arbitrary provider strings cannot become PostHog properties.

## Migration and switch

Apply `apps/backend/config/migrations/aa-observability-v1.sql`, followed by
`aa-observability-v2.sql`, **before** a new API/keeper image. They add nullable
diagnostic columns and separate observation/diagnostic tables, including
restart-safe correlation lease metadata; they do not replace authorization tables.
The runtime roles need the documented table privileges. No startup DDL is added.

`enable_aa_readiness_enforcement` defaults to false and maps to
`AA_READINESS_ENFORCEMENT_ENABLED`. Terraform rejects enabling it outside the
Frankfurt trading canary. Keep it false through qualification. It is not an
authorization bypass and does not stop existing recovery.

Rollback disables readiness enforcement and restores prior images. Keep additive
tables/columns and existing operation journals. Do not delete signed preparations
or unresolved diagnostic mappings to reset a test.

## Remaining engineering work

This is **not** the entire approved plan:

- Extend funding evidence beyond the keeper to every Alto executor, oracle,
  liquidation, protection and LP-settlement worker; account for each worker's
  outstanding transaction liabilities. The keeper now estimates its idle reserve
  independently; pending nonce gaps remain unknown until reconciled rather than
  claiming their unknown liabilities are zero.
- Complete action-specific live/FAD/frozen price-payload readiness. The current
  lens signal is deliberately insufficient to certify special-mode exits.
- Finish verified final trade outcomes, stage durations and seven-day
  terminal-record pruning. Current
  diagnostics retain unresolved records; automatic retention cleanup is not
  implemented. Late materialization no longer loses the order-to-operation link.
  The keeper's durable latest error is recoverable; a complete per-stage failure
  history is still needed when several different errors precede materialization
  or a successful retry clears that latest error.
- Complete per-attempt, per-stage failure deduplication and sixty-second recurring
  outage summaries beyond frontend readiness, exporter drop accounting, and all
  worker failure projections.
  Existing CloudWatch rate-limited operational summaries are preserved.
- Finish final trade progress/correlation in activity, clock-skew qualification,
  and comprehensive fault-injection tests for every lifecycle stage.
- Provision and validate Frankfurt-filtered PostHog dashboards in project 208816
  after the new schema has actually been ingested. Logs and product events are
  separate datasets; do not treat a UserOperation confirmation as trade execution.

## Verification performed

- Frontend: full unit suite passed (1,340 tests), including distinct readiness
  failures, sixty-second summaries, recovery/unknown evidence and label redaction.
  TypeScript and ESLint passed.
- Backend: keeper executable builds; 1,088 unit examples pass with local mock RPC
  ports enabled. The native-AA PostgreSQL suite passes against isolated PostgreSQL
  16, including migration, durable correlation, lease retries and rollback reads.
- Idle-funding follow-up: sixteen tests cover first-trade independence, reserve
  boundaries, fee spikes, concurrent fixed-block reads, malformed responses,
  reorgs, freshness, pending liabilities and funding recovery.
- Follow-up correlation tests: twelve receipt/identity/malformed-bundle unit cases
  and twelve native-AA PostgreSQL integration examples pass, including competing
  instances, recovered leases, stale-worker fencing and unchanged ledger entries.
- Proxy: readiness/diagnostic authentication and no-cache tests pass alongside
  existing proxy tests; redirect and deployment-validator tests pass.
- Terraform validation and all thirty-one mocked Frankfurt plans pass, including
  rejection of readiness enforcement before Frankfurt trading-canary activation.
- Lua privacy fixtures and a real Fluent Bit routing test pass with synthetic
  records and network disabled. The test asserts exactly one CloudWatch copy and
  one sanitized PostHog copy, including preservation of alarm fields.

## Deployment-dependent acceptance — not executed

After the remaining engineering work and separate release approval:

1. Apply the additive migration and deploy only to Frankfurt, observation-only.
   Use localhost for frontend testing. Verify consent and project 208816 routing.
2. Reproduce insufficient keeper funding, stale/unknown readiness, blocked opens
   and allowed exits, delayed signatures, ambiguous submission, browser closure,
   refreshes, multiple tabs and partial deposits. Do not change funding limits.
3. Inject every lifecycle and exporter failure. Confirm redaction, durable
   correlation, deduplication, backend-only outcomes and unchanged CloudWatch alarms.
4. Capture repeatable two-minute idle-modal HARs before/after and require at least
   50% fewer HTTP `/rpc` requests without reducing mandatory freshness.
5. Run three sets of 100 fresh mixed-action preparations within existing budgets.
   Keep failures/timeouts in each report. Every warm run must meet p50 <=500 ms
   and p95 <=1 second; report cold and retry paths separately.
6. Separately verify sponsored deposit, open and close and safe budget release.
   Only then approve readiness blocking for the Frankfurt canary.

No Singapore deployment, hosted frontend, Core configuration/deployment, new
funding allowance, automatic refill or production rollout is included.
