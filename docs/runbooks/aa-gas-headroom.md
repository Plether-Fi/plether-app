# Native AA execution-gas headroom

Policy: `execution-headroom-v1-150pct-min100000`. Fresh native preparation applies
`max(ceil(estimate * 1.5), estimate + 100000)` once to Alto's execution gas. A result
above 2,000,000 is rejected, not clipped. Other gas fields and economic caps remain
unchanged. The padded payload is persisted before reservation/signing. Old signed
operations continue exact-payload submission/recovery; they are never upgraded in
place. A preparation under the new policy needs a newly reviewed preparation ID.

## Incident and regression fixture

Reported transaction:
`0xa2be35e8ce226cf9c183d5937b9bd873cde7a956b33262c71f3004093362ed16`,
Arbitrum Sepolia block 308224346. The account's deposit batch exhausted execution
gas inside TerminalNavBookV2. Read-only historical execution failed at 419155 gas
and succeeded at 628733 gas. Neither low worker reserves nor current trading
readiness is evidence of that historical cause.

`scripts/fixtures/aa-deposit-gas-20260912.json` contains public contract bytecode
and touched storage at the end of that block. Additional successful-deposit and
order/protection trace paths expand the touched-slot set. It is NOT a complete
chain state snapshot. No production keys, owner signatures or paymaster
authorizations are stored. Addresses and balances in this test asset are public
historical chain data, not analytics payloads.

The offline test starts its own loopback-only Anvil, installs this public state,
and substitutes deterministic test account/paymaster signers locally. It executes
real handleOps and verifies UserOperationEvent success plus the exact EntryPoint
deposit deduction. Captured Core runtime hashes must match the v1.2.3 manifest.
EntryPoint and paymaster runtime hashes are pinned separately. No live network or
wallet credential is needed by CI. Missing Anvil or fixtures fails rather than skips.

Run `node --test scripts/aa-execution-regression.test.mjs` after installing frontend
dependencies and Foundry v1.5.1. The existing Tests/test job now requires this suite.
Use `ARBITRUM_SEPOLIA_RPC_URL=... node scripts/aa-capture-deposit-regression.mjs
--capture --refresh` only when deliberately refreshing public historical fixtures;
the capture tool never broadcasts. Review fixture changes, not just regenerated
success expectations.

## Pre-verification gas and fees

Alto v1.2.7's `preVerificationGasCalculator.ts` fills fixed-width gas words with
maximum values when computing execution calldata overhead. Arbitrum's L1-fee
estimate separately randomizes gas quantities below 10,000,000, above our reviewed
2,000,000 limit. Padding does not change encoded lengths. Preserve this reviewed
estimator, including its existing PVG buffers and validation, rather than adding
a second preparation RPC. Arbitrum compression/fee variability still requires a
live compatibility gate; local Anvil is not an Arbitrum fee oracle.

Structured `aa_preparation_gas_headroom` logs contain estimated/prepared call gas
and headroom basis points. `aa_recovery_outcome` exports total gas utilization
against the operation's total gas allowance, not callGasLimit alone. Receipt gas
includes verification, PVG and EntryPoint unused-gas penalties: headroom is not
necessarily free. CloudWatch retains operational data; PostHog receives only
allowlisted ratios, durations, reasons and opaque attempt references.

## Historical failure enrichment

The API background diagnostic worker claims at most four safely reconciled failed
operations, with a five-minute retry lease and a four-second per-item timeout.
It checks the receipt and canonical block around a bounded callTracer request.
Only one exact EntryPoint-to-account calldata match with a continuously failed
out-of-gas call path supports `USER_OPERATION_OUT_OF_GAS`. Caught errors, empty
revert data and high gas use alone do not establish this cause. Unknown execution
reverts stay `USER_OPERATION_REVERTED`. Database compare-and-set completion
deduplicates emission; no budget lock spans RPC. This uses existing observability
v1/v2 columns and introduces no migration or authorization-table change.

`aa_execution_diagnosed` plus `USER_OPERATION_OUT_OF_GAS` drives the new operations
alarm. Traces, payloads, addresses, exact balances and fees are never exported to
PostHog. Diagnostic outages leave signing, recovery and reconciliation unchanged.

## Qualification and rollout

Implemented tests include the exact gas failure/success, repeat deposits, several
amounts, existing-position/carry state, counterfactual creation, two operations
warming storage in one bundle, token balances changing after signing, and genuine
insufficient-token failures. A valid full-position close commit succeeds. Historical
opens reject with `CloseOnlyWindow`, and protection creation rejects with
`ConditionalTriggerFrozen`; those tests must not silently pass on an unrelated
revert. A successful close commit is NOT a completed trade. A full positive trading
and protection matrix, broader claim/debt cases and live qualification must not be
inferred from these tests. A paired identical-state test also confirms that more
unused execution allowance increases the real EntryPoint charge.

Before release readiness, complete the remaining positive execution matrix;
deploy the reviewed backend and frontend to Singapore Sepolia through protected
workflows; verify representative deposits and permitted open/close outcomes and
safe reconciliation; verify telemetry ingestion; then run three 100-preparation
mixed-action runs within existing budgets. Preserve failures/timeouts and report
market-closed gates as pending. Require p50 <=500 ms and p95 <=1 s per warm run.
The builder test requires exactly one nonce RPC and two Alto RPCs, with no extra
simulation in signing/submission. No Core deployment, spending-cap increase,
automatic funding, provider-policy change or removal of recovery is authorized.

If compatibility fails, do not claim release readiness or silently remove the
headroom/raise gas caps. Pause new issuance through the existing reviewed operator
process if necessary while preserving exact-payload recovery and liabilities.
