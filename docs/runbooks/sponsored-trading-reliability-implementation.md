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
- One shared visible-UI readiness poll, Trading status in native-AA trade review
  and account activity, and an explicit refresh action. Preparation starts the
  read concurrently. Only a fresh blocked result with enforcement enabled stops
  signing; unknown, expired, failed and slow observations do not.
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
- Keeper execution-attempt correlation checks the exact release registration
  and following matching EntryPoint event, rather than assuming the first event
  in a bundle belongs to the order. Historical reasons are never inferred from
  current readiness. The UI reports when a verified cause is unavailable.
- A separate PostHog log projection allowlists categories, counters, random
  references and service resources; raw CloudWatch operational fields remain on
  their original copy. Malformed projection input fails closed. Console capture,
  consent and replay settings are unchanged. Exporter exceptions cannot escape
  frontend capture helpers into signing/recovery.
- Local Frankfurt analytics receive `deployment_name=sepolia-aa-temp`.
  Existing production dashboards were not modified. PostHog project 208816 has
  no existing Frankfurt dashboard; new dashboard/data qualification is pending.

## Migration and switch

Apply `apps/backend/config/migrations/aa-observability-v1.sql` **before** a new
API/keeper image. It adds nullable diagnostic columns to `aa_preparations` and
separate observation/diagnostic tables; it does not replace authorization tables.
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
  outstanding transaction liabilities. The current keeper observer reuses a
  short-lived actual transaction quote and reports unknown when no quote exists.
- Complete action-specific live/FAD/frozen price-payload readiness. The current
  lens signal is deliberately insufficient to certify special-mode exits.
- Finish durable, race-recoverable order-to-attempt correlation, verified final
  trade outcomes, stage durations and seven-day terminal-record pruning. Current
  diagnostics retain unresolved records; automatic retention cleanup is not
  implemented. An asynchronous materialization/indexing race can leave a cause
  unavailable rather than attach a guessed cause.
- Complete per-attempt, per-stage failure deduplication and sixty-second recurring
  outage summaries, exporter drop accounting, and all worker failure projections.
  Existing CloudWatch rate-limited operational summaries are preserved.
- Finish final trade progress/correlation in activity, clock-skew qualification,
  and comprehensive fault-injection tests for every lifecycle stage.
- Provision and validate Frankfurt-filtered PostHog dashboards in project 208816
  after the new schema has actually been ingested. Logs and product events are
  separate datasets; do not treat a UserOperation confirmation as trade execution.

## Verification performed

- Frontend: full unit suite passed (1,332 tests at that run); subsequent focused
  AA/analytics suite passed (242 tests, including two additional exporter tests).
  TypeScript and ESLint passed before the last test-only additions.
- Backend: API executable builds; 1,060 unit examples pass with local mock RPC
  ports enabled. The native-AA PostgreSQL suite passes against isolated PostgreSQL
  16, including migration, durable correlation, lease retries and rollback reads.
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
