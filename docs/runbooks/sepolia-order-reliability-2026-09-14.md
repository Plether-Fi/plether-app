# Sepolia bounty drift and keeper deferrals

## Incident and scope

Two sponsored open commits reverted with
`OrderLifecycleBook__ExecutionBountyAboveBound(uint256,uint256)`:

| Quoted maximum (micro-USDC) | Required bounty (micro-USDC) |
| --- | --- |
| 119061 | 119064 |
| 118746 | 118750 |

The affected transactions were
`0xfb19ad2dd087cddfe4cb7e71dbcb00a3c1e66f46b6c8501c25e751f69b584ecf`
and `0xf3c10ea140cc804219a164990268efc50476387d85e56027bcaa2d2b64105fb8`.
These were bounty-bound reverts, not sponsored execution-gas exhaustion.

Orders 35, 40, 41, 56, 57, 61 and 84 expired. Order 35 had seven seconds
remaining at inclusion; its payload became available at the deadline. Keeper
logs recorded EngineFailure for 40/56 and initial SameBlock deferrals for
41/57/84. Global log suppression prevents complete historical attribution;
there is no retained preflight reason for 61. Do not label all seven as gas
failures or claim these old orders can be revived.

## Changes

- Newly reviewed web requests permit `quote + max(ceil(quote / 100), 10)`
  micro-USDC as the execution reward maximum. This is a ceiling, not the
  actual charge. Show six-decimal precision in execution protections.
- Funding and Max-size review reserve this maximum. Existing relaxed web
  accounting bounds remain unchanged; no additional debit permission is added
  elsewhere. Exact persisted requests are never re-toleranced or replaced.
- The keeper now probes typed EngineFailure outcomes with bounded gas
  escalation, just as it already did for explicit InsufficientGas. The existing
  30M **keeper transaction** cap is unchanged; the sponsored operation cap and
  spending budgets are not changed. A persistent engine failure at the cap
  remains pending, with no broadcast. SameBlock is not treated as a gas issue.
- Per-order/per-reason observations replace global suppression for typed
  preflight deferrals and missing historical payloads. Emit first observations,
  cumulative summaries every 60 seconds, and a terminal summary. Batch stop
  reasons are attributed only to the blocked head, not to later orders.
- CloudWatch records the order locator, first/last observation, count, last
  gas allowance, deadline and payload timestamp. PostHog receives only fixed
  reason codes, relative durations, counts and a random diagnostic UUID.
  This UUID identifies the deferral series, not the original AA preparation.
- The advisory cache is bounded to 4096 order/reason entries with a one-hour
  idle TTL. Cache eviction is logged. A restart loses unflushed aggregate
  counts, not already-emitted CloudWatch evidence or any recovery/ledger data.
  Terminal summaries cover observations retained by that process; they do not
  claim to be a complete cross-restart history.

No new frontend preparation/submission RPC calls, Core changes, funding,
deployment, or changes to signed deadlines are included. Existing 20-second
signing and 10-second submission guards are retained. Delayed signatures remain
journaled but unsubmitted when those guards fail. A new attempt requires review.

## Offline reproduction

`scripts/aa-lifecycle-regression.mjs` exports immutable Core v1.2.3 source
`ffe45937b7f38133133ad292c5435828bf99357d`, plus its pinned dependencies,
into a temporary directory. It runs real EntryPoint/account/paymaster execution
with synthetic local market inputs. It never forks an RPC or loads live keys.

The bounty cases recreate both exact error arguments, then verify a successful
commit inside tolerance and rejection one micro-USDC above the maximum. Actual
reserved bounty must equal the quote at inclusion, not the reviewed maximum.
These are deterministic equivalents, not full historical-state replays.

The keeper fixture reproduces a successful EVM call returning EngineFailure
at 3.25M gas and execution at 3.5M for the same close. It also covers SameBlock
and an injected genuine engine revert that stays pending at the 30M cap.
This establishes the failure mode, not every historical expiry's exact cause.

## Verification and release gates

Local verification on 2026-09-14: frontend full unit suite 1459 passing tests
(plus 78 passing focused tests after the final display/change-list regression),
backend full unit suite 1153 passing examples, recorded-state AA suite 15/15,
immutable Core lifecycle suite 9/9, real offline Fluent Bit routing 6/6,
and Lua privacy fixtures pass. TypeScript and changed-file ESLint pass.
The lifecycle suite includes SameBlock clearing and genuine EngineFailure
remaining pending at the hard cap. No live release qualification has run.

Run frontend unit tests, TypeScript/lint, backend unit tests, recorded-state AA
execution regressions, the immutable Core lifecycle suite, Lua privacy fixtures
and offline Fluent Bit routing tests. The lifecycle suite is mandatory in CI.

After separate deployment approval, deploy the backend/keeper and log router,
then the Sepolia frontend through the existing manual workflows. Confirm:

1. Representative open/close outcomes, actual bounty and safe reconciliation.
2. EngineFailure probes either lead to a verified terminal preflight or remain
   unbroadcast at the cap; no pending result is treated as execution success.
3. Per-order reasons survive normal terminal processing and reach the sanitized
   PostHog stream. Check CloudWatch original alarm fields are unchanged.
4. Measure inclusion-to-payload, eligibility, preflight and settlement timings
   before selecting a larger minimum submission reserve. Offline execution
   time alone is not a valid production deadline budget.
5. Confirm unchanged sponsorship preparation/submission round-trip counts and
   preparation performance. Do not count market-closed live actions as passed.

Old tabs and journaled operations keep their original bounds. Rolling back the
frontend does not rewrite already-issued orders. Keeper rollback restores the
old deferral behavior and may reintroduce avoidable expiries; recovery and
canonical finalization remain available throughout.
