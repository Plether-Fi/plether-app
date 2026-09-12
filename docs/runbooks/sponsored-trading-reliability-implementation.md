# Sponsored trading reliability — implementation status

Status: partial implementation, not release-qualified. No deployment or funding
change is authorized by this document. Core v1.2.3, the 60-second order window,
KMS, budget authority, explicit Sepolia rollout policy and safe confirmation remain unchanged.

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
  remains subject to Singapore release verification.
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
- Sepolia analytics use `deployment_name=sepolia` without changing consent,
  replay or console capture settings. Live dashboard/ingestion qualification remains pending.
- Readiness telemetry emits each distinct component/reason immediately, deduplicates
  evidence shared across actions and summarizes repeated observations every sixty
  seconds while polling is active. Recovery flushes unreported counts. Unknown or
  missing evidence never declares a dependency recovered. Exported labels use
  exact allowlists; arbitrary provider strings cannot become PostHog properties.

## Release and rollback

Follow [Singapore Sepolia AA release](singapore-sepolia-aa-release.md) for migration
ordering, Core bindings, liability recovery, KMS transition and approval gates.

Readiness enforcement remains observation-only until verified. The only enforcement
switch is enable_aa_readiness_enforcement. Rollback disables it and new issuance;
preserve exact-payload submission/recovery and all additive records.

## Unfinished release qualification

- Singapore all-worker funding observations and action-specific oracle readiness.
- Public safe-mode Alto compatibility with Alchemy and the pinned account/paymaster.
- Sanitized PostHog ingestion, outage/deduplication behavior and release dashboards.
- Three 100-preparation warm runs, cold/retry samples and sponsored deposit/open/close.
- At least 50% fewer RPC requests in the repeatable two-minute idle-modal capture.

No release-readiness claim follows from local tests or historical deployment results.
