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

## Deployment and measured result

PR #268 merged as `9bf56aa0db9e5d9004dfe1295daee2b72e18a563` after all eight
checks passed. Local verification also passed 14 PostgreSQL native-AA integration
examples and 17 execution/benchmark tests (including 14 real-contract cases).
Backend workflow [34748540910](https://github.com/Plether-Fi/plether-app/actions/runs/34748540910)
succeeded at that exact commit. API task revision 61 and reconciler revision 10
reached steady state; API image readback matched the commit and public health
returned HTTP 200. No additional frontend or Terraform deployment was needed.

Three fresh unsigned deposit preparations through the same frontend runtime
measured 3,244 ms cold-client, 2,130 ms warm and 2,122 ms warm. Each warm sample
made one browser preparation request. Warm backend totals were 1,924 and 1,887 ms:
security 658–663 ms, fees/nonce/estimation 502–546 ms, and four mandatory
canonical boundaries approximately 160–170 ms each. KMS took 7 ms. Exact-block
account/runtime evidence hit the cache. The two warm observations improved by
approximately 35–39% against the earlier two, but this small preflight is not a
statistical qualification run and still fails the latency target.

The three operations were not signed or submitted. Their private journals remain
intact and their reservations must expire through normal safe reconciliation.
Do not clear or replace these records to accelerate qualification.

## Close qualification findings

A 100-token close was rejected by Alto with verified Core
`OrderRouter__CommitValidation(11)`, before reservation, signing or submission.
The harness had checked token quantization but omitted the frontend's separate
minimum-notional check. A read-only 200-token commit preflight also rejected it.
Core's current minimum notional is 1,000 USDC; at the observed reference price,
the smallest valid quantized partial close is 1,100 tokens. The harness now uses
the existing frontend commit preflight, without changing production RPC behavior.

A freshly reviewed 1,100-token close passed that preflight and Alto simulation.
Alto estimated 1,349,330 call gas; the approved 50% headroom requires 2,023,995.
The existing 2,000,000 cap correctly rejected the operation before reservation
or owner signing. No close transaction was broadcast and the test position was
not changed. This is not an out-of-gas transaction or a successful close test.
The gateway's generic error obscures this distinction; the cause above was
verified from the exact bounded Alto log and checked-in Core error definition.

Increasing the execution-gas cap, reducing headroom or clipping the result has
not been performed. A separate explicit decision is required for a cap change;
ETH spending caps and other authorization checks remain unchanged.

At the latest readiness check, deposit/close service components and all six
funding roles were ready. Opens remained blocked by current execution mode and
protection triggers were unavailable. Readiness does not guarantee that a
particular operation fits the gas cap. Positive close execution, market-dependent
open/protection execution, the three mixed-action 100-preparation runs and the
two-minute RPC-reduction gate remain unqualified. This deployment is not a claim
that all release acceptance criteria passed.
