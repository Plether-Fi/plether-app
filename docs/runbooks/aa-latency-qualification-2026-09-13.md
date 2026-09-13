# Singapore native preparation latency follow-up

## Measured baseline

The deployed gas-headroom release `ecf8ad79d704817f5586696ee21637bcd6a1811f`
prepared three unsigned deposit intents through the actual frontend runtime in
4,234 ms (cold client), 3,514 ms and 3,253 ms (warm client). These samples include
account/factory discovery inside preparation, but not initial wallet connection,
owner signing or submission. They are a small diagnostic preflight, not the
required three mixed-action runs of 100 preparations.

Warm Server-Timing recorded 801–887 ms initial security evidence, 503 ms
fees/nonce/estimation, approximately 320–326 ms for each labeled canonical
delivery check, and 6–7 ms KMS signing. Some mandatory canonical checks were not
individually labeled; stage durations are nested and must not be summed blindly.
The three unsigned authorizations expired through normal safe reconciliation.
No ledger cleanup, broadcast, spending-cap increase or funding was used.

## First optimization

- Run chain-identity and canonical-safe-snapshot reads concurrently. Both must
  succeed before account authorization, estimation or reservation proceeds.
- Run cached/exact-block profile evidence and fresh on-chain pause reads
  concurrently, retaining the subsequent explicit canonical-header check.
- At every existing revalidation boundary, fetch the new safe head and the
  already-selected explicit block concurrently. Preserve regression, changed-hash,
  provider-disagreement, freshness and future-clock checks. No boundary is removed.
- Reuse successful approved account-runtime evidence only for its exact block
  hash, account and approved runtime hash, in the existing bounded per-gateway
  account cache. Provider/configuration scope and snapshot invalidation remain
  unchanged; failed reads and permission are not cached.
- Label the existing before-reservation and after-signing canonical checks so
  the remaining critical path is measurable.

No RPC call, extra estimation, new API, migration, provider configuration change,
KMS replacement, budget adjustment or contract modification is introduced.
No signature is delivered before the existing mandatory checks have passed.

## Verification and acceptance

Deterministic RPC-server barriers verify concurrent boundary reads with exactly
two requests in single-provider mode and four in dual-provider mode. Negative
cases cover regressed safe heads, same-height reorgs, canonical reorgs, wrong
explicit block numbers, stale/future evidence and provider disagreement.

The backend suite passes 1,121 examples locally. Real-contract gas-headroom and
native-AA PostgreSQL regressions remain required in CI. Deployment and new live
measurement must be recorded separately; passing unit tests is not evidence of
meeting p50 <=500 ms and p95 <=1 second.

Continue qualification with fresh mixed deposit/open/close intents within the
existing limits. Retain failures/timeouts, keep cold and exact-retry paths
separate, and wait for normal authorization expiry between budget-limited
batches. Current weekend open/protection rejections are not positive execution
coverage. Preserve those pending gates explicitly until the market permits them.

Rollback restores the previous backend image. All existing preparation records,
authorizations, exact-payload submission and recovery remain compatible.
