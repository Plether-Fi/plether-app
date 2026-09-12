# Native preparation latency qualification

## Status and baseline

The fast path is disabled by default. No live performance claim follows from
unit tests or an offline percentile report. Core v1.2.3, maxOrderAge=60, KMS,
economic limits, and safe-confirmation recovery remain unchanged.

The user's Firefox capture beginning 2026-09-11 10:50:42.323 UTC showed:
gas-price request +6.205 s; stub +6.606 s, duration 1.447 s; gas estimate
+8.058 s, duration 0.174 s; final sponsorship +8.239 s, duration 1.189 s.
Preparation from the gas request to final sponsorship was 3.223 s. This single
sample is not a repeatable p50/p95 baseline and excludes earlier frontend work.

Before enabling the new path, collect the standard path baseline with timing
instrumentation. Backend `http_total` and stage timings appear in Server-Timing
and structured `aa_preparation_timing` logs. Frontend
`plether.aa.preparation` performance measures cover the complete runtime
preparation call, including factory discovery, and include failed calls. Keep
wallet signing, bundler submission/inclusion and keeper execution separate.

## Interface and compatibility

`plether_prepareUserOperation` accepts one object with version=1,
preparationId (32-byte hex), chainId=0x66eee, the pinned EntryPoint, sender,
account-encoded callData and optional paired factory/factoryData. No owner
signature, caller fees, nonce, or paymaster authorization is accepted.
It returns version, EntryPoint, the prepared operation (without owner signature)
and its EntryPoint hash. The client binds all returned intent fields before
wallet signing. The normal operation journal and exact-payload sender remain
authoritative.

Preparation IDs derive from the durable local action ID; a timeout must retry
the same ID without standard-method fallback. Server work leases last 60 s;
preparation records expire after five minutes and are retained as tombstones.
Once selected, nonce/fees/gas cannot be rewritten by lease takeover. Existing
authorization deadlines are never extended on retry. Config changes bind to a
different intent fingerprint and therefore reject reuse of an old ID.

## Singapore rollout (separate release approval)

1. Obtain release approval. Follow the [Singapore release procedure](singapore-sepolia-aa-release.md).
2. Apply `apps/backend/config/migrations/aa-preparation-v1.sql` with the reviewed
   Singapore migration mechanism and database-owner role; grant the existing
   runtime database role only SELECT/INSERT/UPDATE on the new table as needed.
   No startup DDL is added. Verify the table and constraints before activation.
3. Deploy backend instrumentation with preparation disabled. Collect baseline.
4. Enable `enable_native_aa_preparation` / `AA_NATIVE_PREPARATION_ENABLED` on the
   Singapore Sepolia API. Keep existing cohort, sponsorship/submission
   gates and funding ceilings. Public access requires the separate public-release gates.
5. Only after backend health checks, add `preparationRpcVersion: 1` to the
   Sepolia native manifest. Standard manifests and Pimlico stay unchanged.
6. Record 100 fresh preparations per run, for three runs (34 deposits, 33 opens,
   33 closes). Use valid reviewed intents for the current account state. Do not
   bulk-submit trades to manufacture samples. Wait for normal reservation
   release between budget-limited batches; never clear the ledger or bypass
   rate limits. Obtain approval for any extra funds rather than exceeding limits.
7. Export sanitized samples with `run`, `kind` (warm-new/cold-new/retry), `action`,
   `outcome` (success/error/timeout) and complete frontend `durationMs`.
   Record cold and retry samples separately. Keep errors/timeouts in the export.
   Evaluate with `node scripts/aa-preparation-benchmark.mjs samples.json`.
8. Require each warm run p50<=500 ms, p95<=1000 ms, with no errors/timeouts, then
   separately verify sponsored open and close receipts and keeper execution.
   Record source SHA, deployment digest, timestamps, budget usage and results.

If the gate fails, retain the measured bottleneck and keep the new capability
disabled; do not substitute cached retries for fresh operations. Roll back by
removing the manifest capability and disabling new preparation requests. Keep
the additive table and normal sponsorship/recovery data; already signed
operations continue through the existing submission and reconciliation paths.
With sponsorship still enabled, an identical retry may retrieve its already
signed preparation while the preparation flag is off. It cannot reserve or sign
a new authorization. The emergency sponsorship gate still denies delivery.

## Local verification versus release qualification

Local backend unit and PostgreSQL integration suites, frontend AA tests and
type-checking, Terraform mocked tests, and the offline benchmark evaluator have
passed. These establish regression coverage, not a latency result. The repeatable
instrumented baseline, three Singapore warm runs, cold/retry measurements, and
live sponsored open/close remain required after release approval. No deployment,
capability activation, funding or Core change is performed by these local tests.
